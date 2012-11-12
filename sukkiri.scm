;;; sukkiri.scm -- A metadata store based on Redis
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD License. See the accompanying LICENSE file for details.
;;;
;;; DB Administration & Sessions
;;; ============================
;;; Although you may use Sukkiri to store and query data with a simple
;;; connection to any Redis server (i.e., using REDIS-CONNECT from
;;; the redis-client extension, you may wish to segregate your data
;;; from other applications that may be using the same server. To
;;; facilitate this process, this library provides the INIT-SUKKIRI-DBS,
;;; START-SUKKIRI-SERVER, and OPEN-SUKKIRI-DB procedures. The egg also
;;; includes the 'sukkiri-admin' program, which provides a command-line
;;; interface to the first two of these procedures.

(module sukkiri
        (add-prop
         update-prop
         delete-prop
         query
         open-session
         :>
         :>>
         :-
         :<)

        (import scheme)
        (import chicken)
        (import extras)
        (import data-structures)
        (import ports)
        (import srfi-1)
        (import srfi-69)

        (use redis-client)
        (use srfi-19)
        (use numbers)
        (use sets)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL PARAMETERS  ---------------------------------------------

;; FIXME -- obviously this should not be hard-coded. Need to figure out how
;;   to query Redis for this info. (redis-config "get" "databases") doesn't
;;   appear to work.
(define *total-dbs* (make-parameter 16))

(define *default-host* (make-parameter "localhost"))

(define *default-port* (make-parameter 6379))

(define *control-db-idx* (make-parameter "1"))

(define *scratch-db-idx* (make-parameter "0"))

(define *connected* (make-parameter #f))

(define *current-app* (make-parameter #f))

(define *sukkiri-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  FIELD TYPE DEFINITIONS  ----------------------------------------

(define field-types (make-hash-table))

(define-field-type string base-type: string)

(define-field-type number base-type: number)

(define-field-type character base-type: character)

(define-field-type boolean base-type: string)

(define-field-type date base-type: date defined-by: srfi-19
                   storage-type: string from-string: secstring->date
                   to-string: date->secstring)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  STRUCTURED CONTENT TYPES  --------------------------------------

(define content-types (make-hash-table))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  DATA TYPES  ----------------------------------------------------

(define-record-type iref (make-iref target) iref?
                    (target iref-target))

(define-record-printer (iref x out)
                       (fprintf out "[IREF: ~S :]" (iref-target x)))

(define-record-type xref (make-xref target) xref?
                    (target xref-target))

(define-record-printer (xref x out)
                       (fprintf out "[XREF: ~S :]" (xref-target x)))

(define-record-type fref (make-fref target) fref?
                    (target fref-target))

(define-record-printer (fref x out)
                       (fprintf out "[FREF: ~S :]" (fref-target x)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  UTILITY FUNCTIONS  ---------------------------------------------

(define (debug-msg . msgs)
  (when (*sukkiri-debug*)
    (with-output-to-port
      (current-error-port)
      (lambda () (apply print msgs)))))

(define (ymd->date y m d)
  (make-date 0 0 0 0 d m y))

(define (ymdhms->date yr mo dt hr mi #!optional (se 0))
  (make-date 0 se mi hr dt mo yr))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  AUTOMATIC DB ALLOCATION  ---------------------------------------

(define (init-dbs #!optional (force #f))
  (let ((db-empty?
          (lambda () (null? (redis-keys "*")))))
    (redis-select (*control-db-idx*))
    (cond
      ((db-empty?) #t)
      (force (redis-flushdb))
      (else (error "Database contains data; unable to initialize.")))
    (redis-sadd "dbs-in-use" (*control-db-idx*))
    (redis-sadd "dbs-in-use" (*scratch-db-idx*))
    (redis-set "@CONTROL-DB" "1")
    (redis-hset "scratch" "db-index" (*scratch-db-idx*))))

(define (set-total-dbs)
  ;; This query works w/ Redis 2.6, not 2.4.
  (let ((result (redis-config "get" "databases")))
    (when (not (null? result))
      (*total-dbs* (string->number (cadr result))))))

(define (first-available-index indices)
  (let ((indices* (map string->number indices))
        (last-idx (- (*total-dbs*) 1)))
    (let loop ((i 2))
      (cond
        ((> i last-idx) #f)
        ((memv i indices*) (loop (+ i 1)))
        (else i)))))

(define (get-db-index app-id)
  (debug-msg "get-db-index")
  (redis-select (*control-db-idx*))
  (let ((exists (car (redis-exists app-id))))
    (and (= exists 1)
         (car (redis-hget app-id "db-index")))))

(define (allocate-db app-id)
  (debug-msg "allocate-db")
  (redis-select (*control-db-idx*))
  (let* ((allocated-dbs (redis-smembers "dbs-in-use"))
         (available-index (first-available-index allocated-dbs)))
    (if available-index
      (let ((index (number->string available-index)))
        (redis-transaction
          (redis-hset app-id "db-index" index)
          (redis-sadd "dbs-in-use" index))
        index)
      (abort "No dbs available."))))

(define (deallocate-db app-id)
  (let ((index (get-db-index app-id)))
    (redis-transaction
      (redis-select index)
      (redis-flushdb)
      (redis-select (*control-db-idx*))
      (redis-hdel app-id "db-index")
      (redis-srem "dbs-in-use" index))))

(define (with-db-select thunk)
  (let ((app (*current-app*)))
    (when (not app)
      (abort "Current app is not set."))
    (let ((idx (get-db-index app)))
      (redis-select idx)
      (thunk))))

(define (open-session #!optional app-id
                   #!key (host (*default-host*)) (port (*default-port*))
                   (force-init #f))
  (debug-msg "open-session")
  (when (not (*connected*))
    (debug-msg "open-session: not connected")
    (redis-connect host port)
    (debug-msg "open-session: redis-connect done")
    (*connected* #t))
  (debug-msg "open-session: connected")
  (set-total-dbs)
  (redis-select (*control-db-idx*)) ; Select control DB
  (debug-msg "open-session: select control db")
  (let ((rs-exists (redis-exists "@CONTROL-DB")))
    (when (= (car rs-exists) 0)
      (debug-msg "open-session: '@CONTROL-DB' doesn't exist")
      (init-dbs force-init))
    (debug-msg "dbs initialized")
    (if app-id
      (begin
        (debug-msg "there is an app id")
        (*current-app* app-id)
        (let ((index (or (get-db-index app-id)
                         (allocate-db app-id))))
          (debug-msg "got index; now select")
          (redis-select index)
          (debug-msg "selected")
          index))
      #t)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GENERIC STORAGE PROTOCOL  --------------------------------------

(define (generate-node-id)
  (let ((base-id (redis-incr "@NODE-ID")))
    (sprintf "@NODE:~X" (car base-id))))

(define (generate-anon-id)
  (let ((base-id (redis-incr "@ANON-ID")))
    (sprintf "@ANONYMOUS:~X" (car base-id))))

;;; ====================================================================
;;; --  Storage  -------------------------------------------------------

(define (store-hash-table id ht)
  (hash-table-walk
    ht
    (lambda (k v)
      (redis-hset id k (prepare-value v)))))

(define (store-list id lst)
  (for-each
    (lambda (elt)
      (redis-lpush id (prepare-value elt)))
    lst))

(define (store-set id set)
  (set-for-each
    (lambda (elt)
      (redis-sadd id (prepare-value elt)))
    set))

(define (store-anonymous-object f obj)
  (let* ((id (generate-anon-id))
         (stored (f id obj)))
    (and stored id)))

(define (store-anonymous-hash obj)
  (store-anonymous-object obj store-hash-table))

(define (store-anonymous-list obj)
  (store-anonymous-object store-list obj))

(define (store-anonymous-set obj)
  (store-anonymous-object store-set obj))

;;; ====================================================================
;;; --  Retrieval  -----------------------------------------------------

(define (retrieve-anonymous-list id)
  (let* ((len (car (redis-llen id)))
         (elts (redis-lrange id "0" (number->string (- len 1)))))
    (map dbstring->any elts)))

(define (retrieve-anonymous-set id)
  (let ((mems (redis-smembers id)))
    (list->set
      (map dbstring->any mems))))

(define (retrieve-anonymous-hash id)
  (let ((h (make-hash-table))
        (fields (redis-hkeys id)))
    (for-each
      (lambda (fld)
        (let ((raw-val (car (redis-hget id fld))))
          (hash-table-set! h fld (dbstring->any raw-val))))
      fields)
    h))
  
;;; ====================================================================
;;; --  Conversion  ----------------------------------------------------

(define (boolean->string b)
  (if b "T" "F"))

(define date->secstring
  (o number->string inexact->exact time->seconds date->time))

(define secstring->date
  (o seconds->date string->number))

(define (string->boolean s)
  (cond
    ((string=? s "T") #t)
    ((string=? s "F") #f)
    (else (error (sprintf "String '~A' does not represent a boolean." s)))))


(define (prepare-value obj)
  (let* ((prefix+conv
           (cond
             ((null? obj) (cons #\- (lambda (_) "")))
             ((boolean? obj) (cons #\b boolean->string))
             ((number? obj) (cons #\n number->string))
             ((date? obj) (cons #\d date->secstring))
             ((char? obj) (cons #\c ->string))
             ((string? obj) (cons #\s identity))
             ((iref? obj) (cons #\i iref-target))
             ((xref? obj) (cons #\x xref-target))
             ((fref? obj) (cons #\f fref-target))
             ((list? obj)
              (cons #\L store-anonymous-list))
             ((hash-table? obj)
              (cons #\H store-anonymous-hash))
             ((set? obj)
              (cons #\S store-anonymous-set))
             (else #f)))
         (prefix
           (and prefix+conv
                (car prefix+conv)))
         (converter
           (and prefix+conv
                (cdr prefix+conv))))
    (and prefix
         converter
         (list->string (cons prefix (string->list (converter obj)))))))

(define (dbstring->any s)
  (let ((first (string-ref s 0))
        (rest (substring s 1)))
    (case first
      ((#\-) '())
      ((#\b) (string->boolean rest))
      ((#\n) (string->number rest))
      ((#\d) (secstring->date rest))
      ((#\c) (string-ref rest 0))
      ((#\i) (make-iref rest))
      ((#\x) (make-xref rest))
      ((#\f) (make-fref rest))
      ((#\s) rest)
      ((#\L) (retrieve-anonymous-list rest))
      ((#\H) (retrieve-anonymous-hash rest))
      ((#\S) (retrieve-anonymous-set rest))
      (else (error "Unknown data type.")))))

(define (get-property obj-id prop-name #!optional (type-def #f))
  (let* ((type
           (if type-def #f (entity-type obj)))
         (type-def
           (or type-def
               (hash-table-ref content-types id)))
         (field-type
           (type-def prop-name))
         (field-def
           (hash-table-ref field-types field-type))
         (convert
           (field-def 'from-dbstring))
         (required?
           (member? prop-name (type-def 'required))))
    (let ((rs (car (redis-hget obj-id prop-name))))
      (cond
        ((and (null? rs) required?)
         (error (sprintf "Required property '~A' is missing." prop-name)))
        ((null? rs)
         '())
        (else (convert rs))))))

(define (set-property obj-id prop-name value #!optional (type-def #f))
  (let* ((type
           (if type-def #f (entity-type obj)))
         (type-def
           (or type-def
               (hash-table-ref content-types id)))
         (field-type (type-def prop-name))
         (field-def (hash-table-ref field-types field-type))
         (valid? (field-def 'validator))
         (convert (field-def 'to-dbstring)))
    (if (valid? value)
      (redis-hset obj-id prop-name (convert value))
      (error
        (sprintf
          "'~A' is not a valid value for property '~A' on object '~A'"
          value prop-name obj-id)))))

(define (get-object id)
  (let* ((type (car (redis-hget id "@CONTENT-TYPE")))
         (type-def (hash-table-ref content-types type))
         (fields (type-def 'fields))
         (result (make-hash-table)))
    (for-each
      (lambda (fld)
        (hash-table-set! result fld (get-property id fld type-def)))
      fields)
    result))

(define (set-object! id obj)
  (let* ((type (entity-type obj))
         (type-def (hash-table-ref content-types type)))
  #f)


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  LOW-LEVEL QUERY API  -------------------------------------------

(define (create-literal-node id content)
  (redis-setnx id content))

(define (create-property id name from to)
  (redis-transaction
    (redis-hsetnx id "name" name)
    (redis-hsetnx id "from" from)
    (redis-hsetnx id "to" to)
    (redis-sadd "name" id)))

(define (create-resource id props)
  (redis-transaction
    (for-each
      (lambda (p)
        (let ((fld (car p))
              (val (cdr p)))
          (redis-hsetnx id fld val)))
      props)))

(define (add-prop s p o)
  (redis-transaction
    (redis-hsetnx s p o)
    (redis-sadd "@SUBJECTS" s)))

(define (update-prop s p o)
  (redis-transaction
    (redis-hset s p o)
    (redis-sadd "@SUBJECTS" s)))

(define (delete-prop s p)
  (redis-hdel s p)
  (when (null? (redis-hkeys s))
    (redis-srem "@SUBJECTS" s)))

(define (query:s:p:* s p _)
  (redis-hget s p))

(define (query:s:p:o s p o)
  (let ((db-result (car (redis-hget s p))))
    (cond
      ((null? db-result) '())
      ((string=? db-result o) (list db-result))
      (else '()))))

(define (query:s:p:o+ s p o+)
  (let loop ((o o+)
             (results '()))
    (if (null? o)
      (reverse results)
      (let ((res (query:s:p:o s p (car o))))
        (if (null? res)
          (loop (cdr o) results)
          (loop (cdr o) (cons (car res) results)))))))

(define (get-o-sel o)
  (cond
    ((list? o) query:s:p:o+)
    ((string=? o "*") query:s:p:*)
    (else query:s:p:o)))

(define (query:s:p+ s p+ o)
  (let ((sel (get-o-sel o)))
    (let loop ((p p+)
               (results '()))
      (if (null? p)
        results
        (let* ((p* (car p))
               (res (sel s p* o)))
          (if (null? res)
            (loop (cdr p) results)
            (let ((res*
                    (map
                      (lambda (o*) (list p* o*))
                      res)))
              (loop (cdr p) (append results res*)))))))))


(define (query:s:* s _ o)
  (query:s:p+ s (redis-hkeys s) o))

(define (query:s:p s p o)
  (let ((sel (get-o-sel o)))
    (map
      (lambda (o*) (list p o*))
      (sel s p o))))

(define (get-p-sel p)
  (cond
    ((list? p) query:s:p+)
    ((string=? p "*") query:s:*)
    (else query:s:p)))

(define (query:s+ s+ p o)
  (let ((sel (get-p-sel p)))
    (let loop ((s s+)
               (results '()))
      (if (null? s)
        results
        (let* ((s* (car s))
               (res (sel s* p o)))
          (if (null? res)
            (loop (cdr s) results)
            (let ((res*
                    (map
                      (lambda (po*) (cons s* po*))
                      res)))
              (loop (cdr s) (append results res*)))))))))
                        
(define (query:s s p o)
  (let ((sel (get-p-sel p)))
    (map
      (lambda (po) (cons s po))
      (sel s p o))))

;; This could be deadly for performance
; (define (all-hash-keys)
;   (filter
;     (lambda (x)
;       (and (not (string=? x "@CONFIG"))
;            (not (string=? x "@SUBJECTS"))
;            (string=? (car (redis-type x)) "hash")))
;     (redis-keys "*")))

(define (all-hash-keys)
  (redis-smembers "@SUBJECTS"))

(define (query:* _ p o)
  (query:s+ (all-hash-keys) p o))

(define (filter-result res filter-spec unique)
  (let ((f
          (case filter-spec
            ((all) (lambda (x) x))
            ((sp) (lambda (x) (list (car x) (cadr x))))
            ((so) (lambda (x) (list (car x) (caddr x))))
            ((po) (lambda (x) (list (cadr x) (caddr x))))
            ((s) (lambda (x) (car x)))
            ((p) (lambda (x) (cadr x)))
            ((o) (lambda (x) (caddr x)))
            (else #f))))
    (cond 
      ((and f unique) (delete-duplicates (map f res)))
      (f (map f res))
      (else '()))))

(define (query s p o #!key (filter 'all) (unique #t))
  (let* ((sel
           (cond
             ((list? s) query:s+)
             ((string=? s "*") query:*)
             (else query:s)))
         (res (sel s p o)))
    (filter-result res filter unique)))

;; Useful (?) aliases
(define :> add-prop)
(define :>> update-prop)
(define :- delete-prop)
(define :< query)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ====================================================================
;;; --------------------------------------------------------------------
