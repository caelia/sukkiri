;;; sukkiri-store-sqlite.scm -- SQLite3 implementation for Sukkiri database layer
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

(module sukkiri-store-sqlite
        (init-store)

        (import scheme chicken)
        (import extras)
        (import files)
        (import data-structures)
        (import ports)
        (import irregex)
        (import srfi-1)
        (use sql-de-lite)
        (use srfi-19)
        (use srfi-19-period)
        (use sukkiri-base)
        (use sukkiri-store)

        (include "sukkiri-common-sql.scm")

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  CURRENT DATABASE  ------------------------------------------------

(define %db-file% (make-parameter #f))

(define %current-db% (make-parameter #f))

(define (connect #!optional (filespec (%db-file%)))
  (if filespec
    (let ((db (open-database filespec)))
      (%current-db% db)
      db)
    (error "No database specified.")))

(define (disconnect)
  (let ((db (%current-db%)))
    (and db
         (close-database db)
         (%current-db% #f))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define iso-format "~Y-~m-~dT~H:~M:~S")

(define << values)

(define db->integer string->number)

(define db->float string->number)

(define db->string identity)

(define (db->boolean dbval)
  (cond
    ((string=? dbval "0") #f)
    ((string=? dbval "1") #t)
    (else (eprintf "'~A' is not a boolean value" dbval))))

(define (db->date dbval)
  (string->date dbval iso-format))

(define (db->time dbval)
  (date->time (string->date dbval iso-format)))

(define db->period string->number)

(define integer->db number->string)

(define float->db number->string)

(define string->db identity)

(define (boolean->db b)
  (if b 1 0))

(define (date->db d)
  (date->string d iso-format))

(define (time->db t)
  (date->string (time->date t) iso-format))

;; Currently a period is just a seconds value
(define period->db identity)

(define validate-integer integer?)

(define validate-float flonum?)

(define validate-boolean boolean?)

(define validate-string string?)

(define validate-date date?)

(define validate-time time?)

(define validate-period number?)

(define (primitive? typespec)
  (memv
    (string->symbol typespec)
    '(integer float boolean string date time period nref rref xref sref)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE SETUP  --------------------------------------------------

(define (@create-db filename)
  (let* ((db
          (open-database filename))
         (qx
          (lambda (q)
            (let ((s (sql/transient db q)))
              (exec s)))))
    (for-each
      qx
      `(,create-primitive-table-query
        ,create-string-type-table-query
        ,create-number-type-table-query
        ,create-vocab-table-query
        ,create-cardinality-table-query
        ,create-struct-type-table-query
        ,create-type-class-table-query
        ,create-types-table-query
        ,create-union-type-table-query
        ,create-struct-members-table-query
        ,create-statement-table-query))
    (for-each
      (lambda (qlist) (for-each qx qlist))
      `(,populate-primitive-table-queries
        ,populate-cardinality-table-queries
        ,populate-type-class-table-queries
        ,populate-types-table-queries
        ,populate-union-type-table-queries))
    (close-database db)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USER-DEFINED TYPE MANAGEMENT  ------------------------------------

(define (@begin-transaction db)
  (let ((st (sql/transient db "BEGIN TRANSACTION;")))
    (exec st)))

(define (@do-query db/file f)
  (let* ((db-obj? (not (string? db/file)))
         (db (if db-obj? db/file (open-database db/file))))
    (if db-obj?
      (f db)
      (begin
        (handle-exceptions
          exn
          (lambda (exn) (rollback db) (close-database db) (abort exn))
          (begin-transaction db)
          (f db)
          (commit db))
        (close-database db)))))

(define (@add-general-type db name class)
  (let ((st (sql/transient db add-type-query)))
    (exec st name class))
  (unless (string=? class "union")
    (update-union-type db "any" members+: `(,name))))

(define (@delete-general-type db name #!optional (union? #f))
  (let ((st (sql/transient delete-type-query)))
    (exec st name))
  (unless union?
    (update-union-type db "any" members-: `(,name))))

(define (@add-string-type db/file name pattern #!optional (description '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db add-string-type-query)))
        (exec st name pattern description))
      (add-general-type db name "string"))))

(define (@add-number-type db/file name #!key (minval '()) (maxval '())
                                             (step '()) (digits '())
                                             (description '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db add-number-type-query)))
        (exec st name minval maxval step digits description))
      (add-general-type db name "number"))))

(define (@add-vocab-type db/file name terms)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql db add-vocab-type-term-query)))
        (for-each
          (lambda (term) (exec st name term))
          terms))
      (add-general-type db name "vocab"))))

(define (@add-struct-type db/file name #!key (extensible #t) (members '())
                                            (description '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st-main (sql/transient db add-struct-type-query))
            (st-mem (sql db add-struct-member-query)))
        (exec st-main name (if extensible 1 0) description)
        (for-each
          (lambda (mem)
            (exec st-mem (symbol->string (car mem)) name (cadr mem) (caddr mem)))
          members))
        (add-general-type db name "struct"))))

(define (@add-union-type db/file name members)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql db add-union-type-member-query)))
        (for-each
          (lambda (mem) (exec st name mem))
          members))
      (add-general-type db name "union"))))

(define (@update-string-type db/file name pattern)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db update-string-type-query)))
        (exec st pattern name)))))

(define (@update-number-type db/file name #!key (minval #f) (maxval #f)
                                               (step #f) (digits #f))
  (do-query
    db/file
    (lambda (db)
      (let* ((st-current (sql/transient db get-number-type-query))
             (st-update (sql/transient db update-number-type-query))
             (current-vals (query fetch-alist st-current name))
             (minval* (or minval (alist-ref 'minval current-vals)))
             (maxval* (or maxval (alist-ref 'maxval current-vals)))
             (step* (or step (alist-ref 'step current-vals)))
             (digits* (or digits (alist-ref 'digits current-vals))))
        (exec st-update minval* maxval* step* digits* name)))))

(define (@update-vocab-type db/file name #!key (terms+ '()) (terms- '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st-add (sql db add-vocab-type-term-query))
            (st-del (sql db update-vocab-type-delete-term-query)))
        (for-each
          (lambda (term) (exec st-add name term))
          terms+)
        (for-each
          (lambda (term) (exec st-del name term))
          terms-)))))

(define (@update-struct-type db/file name #!key (extensible #t) (members+ '())
                                               (members- '()) (members* '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st-ext (sql/transient db update-struct-type-extensible-query))
            (st-add (sql db add-struct-member-query))
            (st-del (sql db delete-struct-member-query))
            (st-current (sql db get-struct-member-query))
            (st-upd (sql db update-struct-member-query)))
        (exec st-ext extensible name)
        (for-each
          (lambda (mem)
            (exec st-add
                  (alist-ref 'rel-name mem)
                  name
                  (alist-ref 'cardinality mem)
                  (alist-ref 'type mem)))
          members+)
        (for-each
          (lambda (mem) (exec st-del name mem))
          members-)
        (for-each
          (lambda (mem)
            (let* ((rel-name (alist-ref 'rel-name mem))
                   (current-values
                     (query fetch-alist st-current name rel-name))
                   (rel-name* (or (alist-ref 'new-rel-name mem) rel-name))
                   (cardinality* (or (alist-ref 'cardinality mem)
                                     (alist-ref 'cardinality current-values)))
                   (mem-type* (or (alist-ref 'mem-type mem)
                                  (alist-ref 'mem-type current-values))))
              (exec st-upd rel-name* cardinality* mem-type* name rel-name)))
          members*)))))

(define (@update-union-type db/file name #!key (members+ '()) (members- '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st-add (sql db add-union-type-member-query))
            (st-del (sql db update-union-type-delete-member-query)))
        (for-each
          (lambda (mem) (exec st-add name mem))
          members+)
        (for-each
          (lambda (mem) (exec st-del name mem))
          members-)))))

(define (@delete-string-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-string-type-query)))
        (exec st name))
      (delete-general-type db name))))

(define (@delete-number-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-number-type-query)))
        (exec st name))
      (delete-general-type db name))))

(define (@delete-vocab-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-vocab-type-query)))
        (exec st name))
      (delete-general-type db name)))) 

(define (@delete-struct-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st-main (sql/transient db delete-struct-type-query))
            (st-mem (sql/transient db delete-struct-members-query)))
        (exec st-mem name)
        (exec st-main name))
      (delete-general-type db name))))

(define (@delete-union-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-union-type-query)))
        (exec st name))
      (delete-general-type db name #t))))

(define (@get-string-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-string-type-query)))
        (query fetch-value st name)))))

(define (@get-number-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-number-type-query)))
        (query fetch-alist st name)))))

(define (@get-vocab-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-vocab-terms-query)))
        (query fetch-column st name)))))

(define (@get-struct-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let* ((st (sql/transient db get-struct-type-query))
             (memspecs*
               (query fetch-alists st name))
             (extensible
               (= (alist-ref 'extensible (car memspecs*)) 1))
             (memspecs
               (map
                 (lambda (ms)
                   `(,(string->symbol (alist-ref 'rel_name ms))
                     ,(alist-ref 'cardinality ms) ,(alist-ref 'mem_type ms)))
                 memspecs*)))
        `(,extensible ,memspecs)))))

(define (@get-union-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-union-type-members-query)))
        (query fetch-column st name)))))

(define (@get-string-types db/file)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-string-types-query)))
        (query fetch-column st)))))

(define (@get-number-types db/file)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-number-types-query)))
        (query fetch-column st)))))

(define (@get-vocab-types db/file)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-vocab-types-query)))
        (query fetch-column st)))))

(define (@get-struct-types db/file)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-struct-types-query)))
        (query fetch-column st)))))

(define (@get-union-types db/file)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-union-types-query)))
        (query fetch-column st)))))

(define (@get-type-class db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-type-class-query)))
        (fetch-value st name)))))

(define (@get-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((cls (get-type-class db name)))
        (case (string->symbol cls)
          ((primitive) (string->symbol name))
          ((string) (get-string-type db name))
          ((number) (get-number-type db name))
          ((vocab) (get-vocab-type db name))
          ((struct) (get-struct-type db name))
          ((union) (get-union-type db name))
          (else (eprintf "Invalid type class")))))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  STATEMENT MANIPULATION  ------------------------------------------

(define (@add-statement db/file s p o t)
  (do-query
    db/file
    (lambda (db)
      (let ((st-add (sql/transient db add-statement-query)))
        (exec st-add s p o t)))))

(define (@add-statements db/file sts)
  (do-query
    db/file
    (lambda (db)
      (let ((st-add (sql db add-statement-query)))
        (for-each
          (lambda (stmt)
            (let ((s (car stmt))
                  (p (cadr stmt))
                  (t* (caddr stmt))
                  (o* (cdddr stmt)))
              (let-values (((t o) (prepare-object db t* o*)))
                (exec st-add s p o t))))
          sts)))))

(define (@delete-statements db/file #!key (s #f) (st #f) (p #f) (o #f) (t #f))
  (let-values (((q-del params)
                  (cond
                    ((and s p o) (<< delete-statements-spo-query `(,s ,p ,o)))
                    ((and s p t) (<< delete-statements-spt-query `(,s ,p ,t)))
                    ((and s p) (<< delete-statements-sp-query `(,s ,p)))
                    ((and s o) (<< delete-statements-so-query `(,s ,o)))
                    ((and s t) (<< delete-statements-st-query `(,s ,t)))
                    ((and p o) (<< delete-statements-po-query `(,p ,o)))
                    ((and p t) (<< delete-statements-pt-query `(,p ,t)))
                    (s (<< delete-statements-s-query `(,s)))
                    (p (<< delete-statements-p-query `(,p)))
                    (o (<< delete-statements-o-query `(,o)))
                    (t (<< delete-statements-t-query `(,t)))
                    (else (error "Invalid arguments for delete-statements.")))))
    (do-query
      db/file
      (lambda (db)
        (let ((st-del (sql/transient db q-del)))
          (apply exec `(,st-del ,@params)))))))

(define (@update-statement-object db/file s p o)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db update-statement-object-query)))
        (exec st s p o)))))

(define (@statement-exists? db/file #!key (s #f) (p #f) (o #f) (t #f))
  (let-values (((q-ex params)
                  (cond
                    ((and s p o) (<< exists-spo-query `(,s ,p ,o)))
                    ((and s p t) (<< exists-spt-query `(,s ,p ,t)))
                    ((and s p) (<< exists-sp-query `(,s ,p)))
                    ((and s o) (<< exists-so-query `(,s ,o)))
                    ((and s t) (<< exists-st-query `(,s ,t)))
                    ((and p o) (<< exists-po-query `(,p ,o)))
                    ((and p t) (<< exists-pt-query `(,p ,t)))
                    (s (<< exists-s-query `(,s)))
                    (p (<< exists-p-query `(,p)))
                    (o (<< exists-o-query `(,o)))
                    (t (<< exists-t-query `(,t)))
                    (else (error "Invalid arguments for statement-exists?.")))))
    (do-query
      db/file
      (lambda (db)
        (let ((st-ex (sql/transient db q-ex)))
          (apply exec `(,st-ex ,@params)))))))

(define (object->ext-type statement)
  (let* ((subject (alist-ref 's statement))
         (prop (alist-ref 'p statement))
         (type (alist-ref 't statement))
         (raw-object (alist-ref 'o statement))
         (object
           (case (string->symbol type)
             ((integer) (db->integer raw-object))
             ((float) (db->float raw-object))
             ((boolean) (db->boolean raw-object))
             ((date) (db->date raw-object))
             ((time) (db->time raw-object))
             ((period) (db->period raw-object))
             (else raw-object))))
    `((s . ,subject) (p . ,prop) (o . ,object))))

(define (@get-statements db/file #!key (s #f) (p #f) (o #f) (t #f))
  (let-values (((q-get params)
                (cond
                  ((and s p t) (<< get-statements-spt-query `(,s ,p ,t)))
                  ((and s p) (<< get-statements-sp-query `(,s ,p)))
                  ((and s o) (<< get-statements-so-query `(,s ,o)))
                  ((and s t) (<< get-statements-st-query `(,s ,t)))
                  ((and p o) (<< get-statements-po-query `(,p ,o)))
                  ((and p t) (<< get-statements-pt-query `(,p ,t)))
                  (s (<< get-statements-s-query `(,s)))
                  (p (<< get-statements-p-query `(,p)))
                  (o (<< get-statements-o-query `(,o)))
                  (t (<< get-statements-t-query `(,t)))
                  (else (error "Invalid arguments for get-statements")))))
    (do-query
      db/file
      (lambda (db)
        (let* ((st-get (sql/transient db q-get))
               (raw-results (apply query `(,fetch-alists ,st-get ,@params))))
          (map object->ext-type raw-results))))))
      
;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(define (init-store filename #!key (create #t) (overwrite #f))
  (create-db @create-db)
  (begin-transaction @begin-transaction)
  (do-query @do-query)
  (add-general-type @add-general-type)
  (delete-general-type @delete-general-type)
  (add-string-type @add-string-type)
  (add-number-type @add-number-type)
  (add-vocab-type @add-vocab-type)
  (add-struct-type @add-struct-type)
  (add-union-type @add-union-type)
  (update-string-type @update-string-type)
  (update-number-type @update-number-type)
  (update-vocab-type @update-vocab-type)
  (update-struct-type @update-struct-type)
  (update-union-type @update-union-type)
  (delete-string-type @delete-string-type)
  (delete-number-type @delete-number-type)
  (delete-vocab-type @delete-vocab-type)
  (delete-struct-type @delete-struct-type)
  (delete-union-type @delete-union-type)
  (get-string-type @get-string-type)
  (get-number-type @get-number-type)
  (get-vocab-type @get-vocab-type)
  (get-struct-type @get-struct-type)
  (get-union-type @get-union-type)
  (get-string-types @get-string-types)
  (get-number-types @get-number-types)
  (get-vocab-types @get-vocab-types)
  (get-struct-types @get-struct-types)
  (get-union-types @get-union-types)
  (get-type-class @get-type-class)
  (get-type @get-type)
  (add-statement @add-statement)
  (add-statements @add-statements)
  (delete-statements @delete-statements)
  (update-statement-object @update-statement-object)
  (statement-exists? @statement-exists?)
  (get-statements @get-statements)

  (%db-file% filename)

  (and
    (cond
      ((and create (not (file-exists? filename)))
        (@create-db filename)
        #t)
      ((and create overwrite (file-exists? filename))
        (delete-file filename)
        (@create-db filename)
        #t)
      ((file-exists? filename)
        #t)
      (else
        #f))
    (let ((connection #f))
      (lambda (cmd . args)
        (case cmd
          ((connect)
           (set! connection (open-database filename))
           connection)
          ((disconnect) 
           (when connection
             (close-database connection)
             (set! connection #f)))
          ((delete)
           (when connection
             (close-database connection))
           (delete-file* filename)))))))

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------
