;;; rel8r-sqlite.scm -- SQLite3 interface for the Rel8r tool.
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

(module rel8r-store
        *
        (import scheme chicken)
        (import extras)
        (import data-structures)
        (import ports)
        (import irregex)
        (import srfi-1)
        (use sql-de-lite)
        (use s11n)
        (use srfi-19)
        (use srfi-19-period)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (eprintf msg . args)
  (error (apply sprintf `(,msg ,@args))))

(define db->integer string->number)

(define db->float string->number)

(define db->string identity)

(define (db->boolean dbval)
  (cond
    ((string=? dbval "0") #f)
    ((string=? dbval "1") #t)
    (else (eprintf ("'~A' is not a boolean value")))))

(define (db->datetime dbval)
  (with-input-from-string dbval
    (lambda ()
      (deserialize))))

(define integer->db number->string)

(define float->db number->string)

(define string->db identity)

(define (boolean->db b)
  (if b "1" "0"))

(define (datetime->db dt)
  (with-output-to-string
    (lambda ()
      (serialize dt))))

(define validate-integer integer?)

(define validate-float flonum?)

(define validate-boolean boolean?)

(define validate-string string?)

(define (validate-datetime dt)
  (or (date? dt)
      (time? dt)
      (time-period? dt)
      (and (list? dt)
           (= (length dt) 2)
           (date? (car dt))
           (time? (cadr dt)))))

(define (primitive? typespec)
  (memv
    (string->symbol typespec)
    '(integer float boolean string date time datetime nref rref xref)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE SETUP  --------------------------------------------------

;;; ------  Queries  -------------------------------------------------------

(define create-primitive-table-query
  "CREATE TABLE primitives (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL
  );")

(define populate-primitive-table-queries
  '("INSERT INTO primitives (name) VALUES ('integer');"
    "INSERT INTO primitives (name) VALUES ('float');"
    "INSERT INTO primitives (name) VALUES ('boolean');"
    "INSERT INTO primitives (name) VALUES ('string');"
    "INSERT INTO primitives (name) VALUES ('date');"
    "INSERT INTO primitives (name) VALUES ('time');"
    "INSERT INTO primitives (name) VALUES ('datetime');"
    "INSERT INTO primitives (name) VALUES ('nref');"
    "INSERT INTO primitives (name) VALUES ('rref');"
    "INSERT INTO primitives (name) VALUES ('xref');"))

(define create-string-type-table-query
  "CREATE TABLE string_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    pattern TEXT NOT NULL,
    description TEXT
  );")

(define create-number-type-table-query
  "CREATE TABLE number_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    minval FLOAT,
    maxval FLOAT,
    step FLOAT,
    digits INTEGER,
    description TEXT
  );")

(define create-vocab-table-query
  "CREATE TABLE vocabs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    term TEXT NOT NULL
  );")

(define create-cardinality-table-query
  "CREATE TABLE cardinalities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );")

(define populate-cardinality-table-queries
  '("INSERT INTO cardinalities (name) VALUES ('one');"
    "INSERT INTO cardinalities (name) VALUES ('zoo');"
    "INSERT INTO cardinalities (name) VALUES ('zoma');"
    "INSERT INTO cardinalities (name) VALUES ('ooma');"))

(define create-struct-type-table-query
  "CREATE TABLE struct_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    extensible INTEGER default 0,
    description TEXT
  );")

(define create-type-class-table-query
  "CREATE TABLE type_classes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );")

(define populate-type-class-table-queries
  '("INSERT INTO type_classes (name) VALUES ('primitive');"
    "INSERT INTO type_classes (name) VALUES ('string');"
    "INSERT INTO type_classes (name) VALUES ('number');"
    "INSERT INTO type_classes (name) VALUES ('vocab');"
    "INSERT INTO type_classes (name) VALUES ('struct');"
    "INSERT INTO type_classes (name) VALUES ('union');"))

(define create-types-table-query
  "CREATE TABLE types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    class INTEGER REFERENCES type_classes(id)
  );")

(define populate-types-table-queries
  '("INSERT INTO types (name, class)
     SELECT 'integer', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'float', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'boolean', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'string', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'date', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'time', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'datetime', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'nref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'rref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'xref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class)
     SELECT 'any', id FROM type_classes WHERE name = 'union';"))
    
(define create-union-type-table-query
  "CREATE TABLE union_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    member_type INTEGER REFERENCES types(id)
  );")

(define populate-union-type-table-queries
  '("INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'integer';" 
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'float';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'boolean';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'string';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'date';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'time';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'datetime';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'nref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'rref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'xref';"))

(define create-struct-members-table-query
  "CREATE TABLE struct_type_members (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    struct_type INTEGER REFERENCES struct_types(id), 
    rel_name TEXT NOT NULL,
    cardinality INTEGER REFERENCES cardinalities(id),
    mem_type INTEGER REFERENCES types(id)
  );")

(define create-triple-table-query
  "CREATE TABLE triples (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    s  TEXT NOT NULL,
    st  INTEGER REFERENCES types(id) NOT NULL,
    p  TEXT NOT NULL,
    o  TEXT NOT NULL,
    ot  INTEGER REFERENCES types(id) NOT NULL
  );")

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define (create-db filename)
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
        ,create-triple-table-query))
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

;;; ------  Queries  -------------------------------------------------------

(define add-string-type-query
  "INSERT INTO string_types (name, pattern, description) VALUES (?, ?, ?);")

(define add-number-type-query
  "INSERT INTO number_types (name, minval, maxval, step, digits, description)
   VALUES (?, ?, ?, ?, ?, ?);")

(define add-vocab-type-term-query
  "INSERT INTO vocabs (name, term) VALUES (?, ?);")

(define add-struct-type-query
  "INSERT INTO struct_types (name, extensible) VALUES (?, ?);")

(define add-struct-member-query
  "INSERT INTO struct_type_members (struct_type, rel_name, cardinality, mem_type)
    SELECT struct_types.id, ?, cardinalities.id, types.id)
    FROM struct_types, cardinalities, types
    WHERE struct_types.name = ?  AND cardinalities.name = ? AND types.name = ?;")

(define add-union-type-member-query
  "INSERT INTO union_types (name, member_type) VALUES (?, ?);")

(define add-type-query
  "INSERT INTO types (name, class) VALUES (?, ?);")

(define update-string-type-query
  "UPDATE string_types SET pattern = ? WHERE name = ?;")

(define update-number-type-query
  "UPDATE number_types SET minval = ?, maxval = ?, step = ?, digits = ?
   WHERE name = ?;")

(define update-number-type-min-query
  "UPDATE number_types SET minval = ? WHERE name = ?;")

(define update-number-type-max-query
  "UPDATE number_types SET maxval = ? WHERE name = ?;")

(define update-number-type-step-query
  "UPDATE number_types SET step = ? WHERE name = ?;")

(define update-number-type-digits-query
  "UPDATE number_types SET digits = ? WHERE name = ?;")

(define update-vocab-type-delete-term-query
  "DELETE FROM vocabs WHERE name = ? and term = ?;")

(define update-struct-type-extensible-query
  "UPDATE struct_types SET extensible = ? WHERE name = ?;")

(define update-struct-type-description-query
  "UPDATE struct_types SET description = ? WHERE name = ?;")

(define update-struct-member-query
  "UPDATE struct_type_members
   SET rel_name = ?, cardinality = ?, mem_type = ?
   WHERE struct_type = struct_types.id
   AND struct_types.name = ? AND rel_name = ?;")

(define update-struct-member-type-query
  "UPDATE struct_type_members SET mem_type = types.id
   WHERE struct_type = struct_types.id AND struct_types.name = ?
   AND rel_name = ? AND types.name = ?;")

(define update-struct-member-cardinality-query
  "UPDATE struct_type_members SET cardinality = cardinalities.id
   WHERE struct_type = struct_types.id AND struct_types.name = ?
   AND rel_name = ? AND cardinalities.name = ?;")

(define update-struct-member-relname-query
  "UPDATE struct_type_members SET rel_name = ?
   WHERE struct_type = struct_types.id AND struct_types.name = ?
   AND rel_name = ?;")

(define update-union-type-delete-member-query
  "DELETE FROM union_types WHERE name = ? and member_type = ?;")

(define delete-string-type-query
  "DELETE FROM string_types WHERE name = ?;")

(define delete-number-type-query
  "DELETE FROM number_types WHERE name = ?;")

(define delete-vocab-type-query
  "DELETE FROM vocab_types WHERE name = ?;")

(define delete-struct-type-query
  "DELETE FROM struct_types WHERE name = ?;")

(define delete-struct-member-query
  "DELETE FROM struct_type_members
   WHERE struct_type = struct_types.id AND struct_type.name = ?
   AND rel_name = ?;")

(define delete-struct-members-query
  "DELETE FROM struct_type_members
   WHERE struct_type = struct_types.id AND struct_type.name = ?;")

(define delete-union-type-query
  "DELETE FROM union_types WHERE name = ?;")

(define delete-type-query
  "DELETE FROM types WHERE name = ?;")

(define get-string-type-query
  "SELECT pattern FROM string_types WHERE name = ?;")

(define get-number-type-query
  "SELECT minval, maxval, step, digits
   FROM number_types WHERE name = ?;")

(define get-vocab-terms-query
  "SELECT term FROM vocab_types WHERE name = ?;")

(define get-struct-member-query
  "SELECT cardinality, mem_type FROM struct_type_members, struct_types
   WHERE struct_type = struct_types.id
   AND struct_types.name = ? AND rel_name = ?;")

(define get-struct-type-query
  "SELECT extensible, rel_name, cardinality, mem_type
   FROM struct_types, struct_type_members
   WHERE struct_types.name = ? AND struct_type_members.struct_type = struct_types.id;")

(define get-union-type-members-query
  "SELECT member_type FROM union_types WHERE name = ?;")

(define get-type-class-query
  "SELECT class FROM types WHERE name = ?;")

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define (begin-transaction db)
  (let ((st (sql/transient db "BEGIN TRANSACTION;")))
    (exec st)))

(define (do-query db/file f)
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

(define (add-general-type db name class)
  (let ((st (sql/transient db add-type-query)))
    (exec st name class)))

(define (delete-general-type db name)
  (let ((st (sql/transient delete-type-query)))
    (exec st name)))

(define (add-string-type db/file name pattern #!optional (description '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db add-string-type-query)))
        (exec st name pattern description))
      (add-general-type db name "string"))))

(define (add-number-type db/file name #!key (minval '()) (maxval '())
                                            (step '()) (digits '())
                                            (description '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db add-number-type-query)))
        (exec st name minval maxval step digits description))
      (add-general-type db name "number"))))

(define (add-vocab-type db/file name terms)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql db add-vocab-type-term-query)))
        (for-each
          (lambda (term) (exec st name term))
          terms))
      (add-general-type db name "vocab"))))

(define (add-struct-type db/file name #!key (extensible 1) (members '())
                                            (description '()))
  (do-query
    db/file
    (lambda (db)
      (let ((st-main (sql/transient db add-struct-type-query))
            (st-mem (sql db add-struct-member-query)))
        (exec st-main name extensible description)
        (for-each
          (lambda (mem)
            (exec st-mem
                  (alist-ref 'rel-name mem)
                  name
                  (alist-ref 'cardinality mem)
                  (alist-ref 'type mem)))
          members))
        (add-general-type db name "struct"))))

(define (add-union-type db/file name members)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql db add-union-type-member-query)))
        (for-each
          (lambda (mem) (exec st name mem))
          members))
      (add-general-type db name "union"))))

(define (update-string-type db/file name pattern)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db update-string-type-query)))
        (exec st pattern name)))))

(define (update-number-type db/file name #!key (minval #f) (maxval #f)
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

(define (update-vocab-type db/file name #!key (terms+ '()) (terms- '()))
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

(define (update-struct-type db/file name #!key (extensible #t) (members+ '())
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

(define (update-union-type db/file name #!key (members+ '()) (members- '()))
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

(define (delete-string-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-string-type-query)))
        (exec st name))
      (delete-general-type db name))))

(define (delete-number-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-number-type-query)))
        (exec st name))
      (delete-general-type db name))))

(define (delete-vocab-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-vocab-type-query)))
        (exec st name))
      (delete-general-type db name)))) 

(define (delete-struct-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st-main (sql/transient db delete-struct-type-query))
            (st-mem (sql/transient db delete-struct-members-query)))
        (exec st-mem name)
        (exec st-main name))
      (delete-general-type db name))))

(define (delete-union-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db delete-union-type-query)))
        (exec st name))
      (delete-general-type db name))))

(define (get-string-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-string-type-query)))
        (query fetch-value st name)))))

(define (get-number-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-number-type-query)))
        (query fetch-alist st name)))))

(define (get-vocab-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-vocab-terms-query)))
        (query fetch-column st name)))))

(define (get-struct-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let* ((st (sql/transient db get-struct-type-query))
             (memspecs*
               (query fetch-alists st name))
             (extensible
               (= (alist-ref 'extensible (car memspecs*))))
             (memspecs
               (map
                 (lambda (ms)
                   `(,(alist-ref 'rel-name ms) ,(alist-ref 'cardinality ms) ,(alist-ref 'mem-type ms)))
                 memspecs*)))
        `(,extensible ,memspecs)))))

(define (get-union-type db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-union-type-members-query)))
        (query fetch-column st name)))))

(define (get-type-class db/file name)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db get-type-class-query)))
        (fetch-value st name)))))

(define (get-type db/file name)
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
;;; ----  VALIDATION FUNCTIONS  --------------------------------------------

(define (validate-primitive-type type value)
  (case type
    ((integer) (validate-integer value))
    ((float) (validate-float value))
    ((boolean) (validate-boolean value))
    ((string) (validate-string value))
    ((date) (validate-datetime value))
    ((time) (validate-datetime value))
    ((datetime) (validate-datetime value))
    ((nref) (validate-string value)) 
    ((rref) (validate-string value)) 
    ((xref) (validate-string value))))

(define (validate-string-type db/file type value)
  (let ((pattern (get-string-type db/file type)))
    (irregex-match pattern value)))

(define (validate-number-type db/file type value)
  (let* ((typespec (get-number-type db/file type))
         (minval (alist-ref 'minval typespec))
         (maxval (alist-ref 'maxval typespec))
         (step (alist-ref 'step typespec)))
    (and (or (null? minval)
             (>= value minval))
         (or (null? maxval)
             (<= value maxval))
         (or (null? step)
             (integer?
                (/ (or (and (null? minval) value)
                       (- value minval)) step))))))

(define (validate-vocab-type db/file type value)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db
                 "EXISTS (SELECT id FROM vocab_types WHERE name=? AND term=?);")))
        (= (query fetch-value st type value) 1)))))

(define (validate-union-type db/file utype subtype)
  (do-query
    db/file
    (lambda (db)
      (let ((st (sql/transient db
                 "EXISTS (SELECT id FROM union_types WHERE name=? AND member_type=?);")))
        (= (query fetch-value st utype subtype) 1)))))

(define (validate-struct-member-cardinality card mlist)
  (case (string->symbol card)
    ((one) (= (length mlist) 1))
    ((zoo) (<= (length mlist) 1))
    ((ooma) (>= (length mlist) 1))
    ((zoma) #t)
    (else (eprintf "Unrecognized value for cardinality: ~A" card))))

(define (validate-struct-member memspec value)
  (let* ((rel-name (car memspec))
         (cardinality (cadr memspec))
         (mem-type (caddr memspec))
         (members 
           (filter
             (lambda (item)
               (equal? (alist-ref 'rel-name item) rel-name))
             value)))
    (and (validate-struct-member-cardinality cardinality members)
         (every (lambda (mem) (validate mem-type mem)) members))))

(define (no-unspecified-members? memspecs value)
  (let ((known-rel-names (map car memspecs)))
    (every (lambda (mem) (member (alist-ref 'rel-name mem) known-rel-names)))))

(define (validate-struct-type db/file type value)
  (let* ((typespec (get-struct-type db/file type))
         (extensible (car typespec))
         (memspecs (cadr typespec)))
    (and (every (lambda (ms) (validate-struct-member ms value)) memspecs)
         (or extensible
             (no-unspecified-members? memspecs value)))))
              
(define (validate db/file type value)
  (if (primitive? type)
    (validate-primitive-type type value)
    (do-query
      db/file
      (lambda (db)
        (let ((cls (get-type-class db type)))
          (case (string->symbol cls)
            ((string) (validate-string-type db type value))
            ((number) (validate-number-type db type value))
            ((vocab) (validate-vocab-type db type value))
            ((union) (validate-union-type db type value))
            ((struct) (validate-struct-type db type value))
            ((*) #t)
            (else (eprintf "Unknown datatype: '~A'" type))))))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO  

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

