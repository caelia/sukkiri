;;; sukkiri-db-base.scm -- Base object definitions for Sukkiri database layer
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD License. See the accompanying LICENSE file for details.
;;;

(module sukkiri-db
        (*total-dbs*
         *default-host*
         *default-port*
         *control-db-idx*
         *scratch-db-idx*
         *connected*
         *current-app*
         allocate-db
         deallocate-db
         with-db-select
         open-db-session)

        (import scheme)
        (import chicken)
        (import extras)
        (import data-structures)
        (import ports)
        (import srfi-1)
        (import srfi-69)

        (import sukkiri-utils)

        (use coops)


(define-class <database>)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL PARAMETERS  -------------------------------------------------

;; Set a reasonable default in case someone is using an older version of
;;   Redis, one that doesn't support CONFIG GET databases. For those who
;;   have an up-to-date version of Redis, this parameter will be set to
;;   the actual configured value at runtime.
(define *total-dbs* (make-parameter 16))

(define *default-host* (make-parameter "localhost"))

(define *default-port* (make-parameter 6379))

(define *control-db-idx* (make-parameter "1"))

(define *scratch-db-idx* (make-parameter "0"))

(define *connected* (make-parameter #f))

(define *current-app* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  UTILITY FUNCTIONS  -------------------------------------------------

(define (db-result->bool rs)
  (case (car rs)
    ((0) #f)
    ((1) #t)
    (else (eprintf "Result '~A' cannot be converted to a boolean value." rs))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  AUTOMATIC DB ALLOCATION  -------------------------------------------

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
    (redis-set "%CONTROL-DB" "1")
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

(define (open-db-session #!optional app-id
                   #!key (host (*default-host*)) (port (*default-port*))
                   (force-init #f))
  (debug-msg "open-db-session")
  (when (not (*connected*))
    (debug-msg "open-db-session: not connected")
    (redis-connect host port)
    (debug-msg "open-db-session: redis-connect done")
    (*connected* #t))
  (debug-msg "open-db-session: connected")
  (set-total-dbs)
  (redis-select (*control-db-idx*)) ; Select control DB
  (debug-msg "open-db-session: select control db")
  (let ((rs-exists (redis-exists "%CONTROL-DB")))
    (when (= (car rs-exists) 0)
      (debug-msg "open-db-session: '%CONTROL-DB' doesn't exist")
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

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GENERIC STORAGE PROTOCOL  ----------------------------------------

(define generate-resource-id (make-parameter #f))

(define resource-id (make-parameter #f))

(define resource-type (make-parameter #f))

(define generate-anon-id (make-parameter #f))

;;; ========================================================================
;;; --  Conversion  --------------------------------------------------------

(define char->string string)

(define (string->char s) (string-ref s 0))

;; We are using our own version of booleans to remove the ambiguity from #t & #f.
(define (boolean->string b)
  (symbol->string b))

(define (string->boolean s)
  (cond
    ((string=? s "%T") %T:)
    ((string=? s "%F") %F:)
    (else (eprintf "String '~A' does not represent a boolean." s))))

(define *converters* (make-parameter '()))

(define (converter-from type)
  (car (alist-ref type (*converters*))))

(define (converter-to type)
  (cadr (alist-ref type (*converters*))))

(define (register-converters type from to)
  (*converters* (cons (list type from to) (*converters*))))

;;; ========================================================================
;;; --  Storage  -----------------------------------------------------------

(define store-property (make-parameter #f))

(define (store-list id lst elt-type)
  (let ((conv (converter-from elt-type)))
    (for-each
      (lambda (elt) (redis-rpush id (conv elt)))
      lst))
  #t)

(define (store-set id set elt-type)
  (let ((conv (converter-from elt-type)))
    (for-each
      (lambda (elt) (redis-sadd id (conv elt)))
      (set->list set)))
  #t)

(define (store-anonymous-object proc)
  (let* ((id (generate-anon-id))
         (stored (proc id)))
    (and stored id)))

(define (store-anonymous-list lst #!optional (elt-type 'string))
  (store-anonymous-object
    (lambda (id) (store-list id lst elt-type))))

(define (store-anonymous-set set #!optional (elt-type 'string))
  (store-anonymous-object
    (lambda (id) (store-set id set elt-type))))

;; Not clear if we need add-to-list/add-to-set--possibly these functions should
;; be done in the UI layer, so I'll keep the functions but not export them
(define (add-to-list id elt #!optional (type 'string))
  (let ((conv (converter-from type)))
    (redis-rpush id (conv elt))))

(define (add-to-set id elt #!optional (type 'string))
  (let ((conv (converter-from type)))
    (redis-sadd id (conv elt))))

;;; ========================================================================
;;; --  Retrieval  ---------------------------------------------------------

(define retrieve-property (make-parameter #f))

(define (retrieve-list id #!optional (elt-type 'string))
  (let* ((conv (converter-to elt-type))
         (len (car (redis-llen id)))
         (raw (redis-lrange id "0" (number->string (- len 1)))))
    (map conv raw)))

(define (retrieve-set id #!optional (elt-type 'string))
  (let ((conv (converter-to elt-type))
        (raw (redis-smembers id)))
    (list->set (map conv raw))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE INDEXES  ------------------------------------------------

(define (index-add! name #!optional (refs '()) #!key (prefix "%HAS-PROP:"))
  (print name)
  (display "REFS: ")
  (pp refs)
  (let ((idx-name (string-append prefix name)))
    (for-each
      (lambda (r) (redis-sadd idx-name r))
      refs)))

(define (index-delete! name value #!optional (prefix "%HAS-PROP:"))
  (let ((idx-name (string-append prefix name)))
    (redis-srem name value)))

(define (index-exists? name value #!optional (prefix "%HAS-PROP"))
  (db-result->bool (redis-sismember name value)))

(define (get-index name #!optional (prefix "%HAS-PROP:"))
  (let ((idx-name (string-append prefix name)))
    (redis-sismember idx-name)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE
