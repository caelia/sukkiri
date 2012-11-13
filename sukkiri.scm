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
        *
;         (register-prop-type
;          register-resource-type
;          xml->register-types
;          create-resource
;          load-proxy-resource
;          delete-resource!
; ;         property-valid?
; ;         resource-valid?
;          open-session
;          prop-responder)

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
        (use irregex)


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
;;; --  PROPERTY TYPES  ------------------------------------------------

(define-syntax make-pt-primitive
  (syntax-rules ()
    ((_ to from val)
     (lambda (msg)
       (case msg
         ((to-string) to)
         ((from-string) from)
         ((validator) val)
         (else #f))))))

; (define prop-types
;   (let ((pt-table (make-hash-table)))
;       pt-table
;       'string
;       (make-pt-primitive identity identity string?))
;     (hash-table-set!
;       pt-table
;       'char
;       (make-pt-primitive
;         (o list->string list) (o car string->list) char?))
;     (hash-table-set!
;       pt-table
;       'number
;       (make-pt-primitive number->string string->number number?))
;     (hash-table-set!
;       pt-table
;       'boolean
;       (make-pt-primitive boolean->string string->boolean boolean?))
;     pt-table))

(define prop-types
  (alist->hash-table
    (list
      (cons 'string
            (make-pt-primitive identity identity string?))
      (cons 'char
            (make-pt-primitive
              (o list->string list) (o car string->list) char?))
      (cons 'number
            (make-pt-primitive number->string string->number number?))
      (cons 'boolean
            (make-pt-primitive boolean->string string->boolean boolean?)))))


(define (make-irregex-validator patt)
  (let ((re (irregex patt)))
    (lambda (str)
      (and (irregex-match re str) #t))))

(define (make-prop-type type #!key (base-type #f) (defined-in #f)
                        (to-string #f) (from-string #f) (validator #f))
  (lambda (msg)
    (case msg
      ((type) type)
      ((base-type) base-type)
      ((defined-in) defined-in)
      ((to-string)
       (or to-string
           (base-type 'to-string)))
      ((from-string)
       (or from-string
           (base-type 'from-string)))
      ((validator)
       (or validator
           (base-type 'validator))))))

(define (register-prop-type type #!key (base-type #f) (defined-in #f)
                            (to-string #f) (from-string #f) (validator #f)
                            (force! #f))
  (when (and (hash-table-exists? prop-types type)
             (not force!))
    (error (sprintf "Property ~A already exists." type)))
  (hash-table-set!
    prop-types
    type
    (make-prop-type
      type base-type: base-type defined-in: defined-in
      to-string: to-string from-string: from-string validator: validator)))

(define (unregister-prop-type type)
  (hash-table-delete! prop-types type))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  RESOURCE TYPES  ------------------------------------------------

(define resource-types (make-hash-table))
 
(define-record prop-spec label type default
               required? element-type min-size max-size)

(define (create-atomic-prop-spec label type #!key
                                 (default '(#f)) (required? #t))
  (make-prop-spec label type default required? #f #f #f)) 

(define (create-struct-prop-spec label type element-type #!key
                                 (default '(#f)) (required? #t)
                                 (min-size 0) (max-size #f))
  (make-prop-spec label type default required? element-type min-size max-size))

(define (register-resource-type type-name prop-specs)
  (hash-table-set! resource-types type-name prop-specs))

(define-record resource id type data)

;(define (create-resource id type #!optional (data '()))
;  (make-resource id type (alist->hash-table data)))

(define (xml->register-types)
  #f)

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
  (let ((rs-exists (redis-exists "%CONTROL-DB")))
    (when (= (car rs-exists) 0)
      (debug-msg "open-session: '%CONTROL-DB' doesn't exist")
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
  (let ((base-id (redis-incr "%NODE-ID")))
    (sprintf "%NODE:~X" (car base-id))))

(define (generate-anon-id)
  (let ((base-id (redis-incr "%ANON-ID")))
    (sprintf "%ANONYMOUS:~X" (car base-id))))

;;; ====================================================================
;;; --  Storage  -------------------------------------------------------

; (define (store-hash-table id ht)
;   (hash-table-walk
;     ht
;     (lambda (k v)
;       (redis-hset id k (prepare-value v)))))
; 
; (define (store-list id lst)
;   (for-each
;     (lambda (elt)
;       (redis-lpush id (prepare-value elt)))
;     lst))
; 
; (define (store-set id set)
;   (set-for-each
;     (lambda (elt)
;       (redis-sadd id (prepare-value elt)))
;     set))
; 
; (define (store-anonymous-object f obj)
;   (let* ((id (generate-anon-id))
;          (stored (f id obj)))
;     (and stored id)))
; 
; (define (store-anonymous-hash obj)
;   (store-anonymous-object obj store-hash-table))
; 
; (define (store-anonymous-list obj)
;   (store-anonymous-object store-list obj))
; 
; (define (store-anonymous-set obj)
;   (store-anonymous-object store-set obj))
; 
; ;;; ====================================================================
; ;;; --  Retrieval  -----------------------------------------------------
; 
; (define (retrieve-anonymous-list id)
;   (let* ((len (car (redis-llen id)))
;          (elts (redis-lrange id "0" (number->string (- len 1)))))
;     (map dbstring->any elts)))
; 
; (define (retrieve-anonymous-set id)
;   (let ((mems (redis-smembers id)))
;     (list->set
;       (map dbstring->any mems))))
; 
; (define (retrieve-anonymous-hash id)
;   (let ((h (make-hash-table))
;         (fields (redis-hkeys id)))
;     (for-each
;       (lambda (fld)
;         (let ((raw-val (car (redis-hget id fld))))
;           (hash-table-set! h fld (dbstring->any raw-val))))
;       fields)
;     h))
  
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


(define (get-property res-id prop-name)
  (let* ((res-type (car (redis-hget res-id "%TYPE")))
         (type-def (hash-table-ref resource-types res-type))
         ;(prop-spec (type-def prop-name))
         (prop-spec (alist-ref prop-name type-def))
         (pt-name (prop-spec-type prop-spec))
         (pt-def (hash-table-ref prop-types pt-name))
         (convert (pt-def 'from-string))
         (required? (prop-spec-required? prop-spec)))
    (let ((rs (car (redis-hget res-id prop-name))))
      (cond
        ((and (null? rs) required?)
         (error (sprintf "Required property '~A' is missing." prop-name)))
        ((null? rs)
         '())
        (else (convert rs))))))

(define (set-property res-id prop-name value #!optional (type-def #f))
  (let* ((res-type (car (redis-hget res-id "%TYPE")))
         (type-def (hash-table-ref resource-types res-type))
         (prop-spec (alist-ref prop-name type-def))
         (pt-name (prop-spec-type prop-spec))
         (pt-def (hash-table-ref prop-types pt-name))
         (valid? (pt-def 'validator))
         (convert (pt-def 'to-string)))
    (if (valid? value)
      (redis-hset res-id prop-name (convert value))
      (error
        (sprintf
          "'~A' is not a valid value for property '~A' on object '~A'"
          value prop-name res-id)))))

(define (get-resource id)
  (let* ((type (string->symbol (car (redis-hget id "%TYPE"))))
         (type-def (hash-table-ref resource-types type))
         (fields (type-def 'fields))
         (result (make-hash-table)))
    (for-each
      (lambda (fld)
        (hash-table-set! result fld (get-property id fld type-def)))
      fields)
    result))

(define (set-resource! obj)
  (let* ((id (resource-id obj))
         (type (resource-type obj))
         (type-def (hash-table-ref resource-types type))
         (data (resource-data obj)))
    (redis-hset id "%TYPE" (symbol->string type))
    (hash-table-for-each
      data
      (lambda (k v)
        (set-property id k v type-def)))))
    


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  PROXY OBJECTS  -------------------------------------------------

; (define-syntax prop-responder
;   (ir-macro-transformer
;     (lambda (expr inj comp)
;       (let* ((res-id (car expr))
;              (prop-name (cadr expr))
;              (prop-type (caddr expr))
;              (type-def (hash-table-ref prop-types prop-type))
;              (ts (type-def 'to-string))
;              (fs (type-def 'from-string))
;              (v (type-def 'validator)))
;         `(lambda (arg . args)
;            (if (null? args)
;              (,fs (car (redis-hget ,res-id ,prop-name)))
;              (let* ((new-val (car args))
;                     (valid? (v new-val)))
;                (if valid?
;                  (redis-hset ,res-id ,prop-name (,ts new-val))
;                  (error "Invalid input!")))))))))

(define (make-prop-responder res-id prop-name prop-type)
  (let* ((type-def (hash-table-ref prop-types prop-type))
         (ts (type-def 'to-string))
         (fs (type-def 'from-string))
         (v (type-def 'validator)))
    (lambda args
      (if (null? args)
        (fs (car (redis-hget res-id prop-name)))
        (let* ((new-val (car args))
               (valid? (v new-val)))
          (if valid? 
            (redis-hset res-id prop-name (ts new-val))
            (error "Invalid input!")))))))

(define (create-resource-proxy id type prop-specs)
  (let ((responders
          (map
            (lambda (p)
              (let ((pn (car p))
                    (pt (cadr p)))
                (cons pn (make-prop-responder id pn pt))))
            prop-specs)))
    (lambda (arg . args)
      (case arg
        ((id) id)
        ((type) type)
        ((resp) responders) ; just for debugging
        (else (apply (alist-ref arg responders string=?) args))))))

(define (create-resource id type . prop-specs)
  (redis-hset id "%TYPE" (symbol->string type))
  (let ((proxy (create-resource-proxy id type prop-specs)))
    (for-each
      (lambda (ps)
        (unless (null? (cddr ps))
          (proxy (car ps) (caddr ps))))
      prop-specs)
    proxy))

(define (load-proxy-resource id)
  #f
  )

(define (delete-resource! id)
  #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ====================================================================
;;; --------------------------------------------------------------------
