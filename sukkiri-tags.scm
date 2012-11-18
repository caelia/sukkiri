;;; sukkiri-lib.scm -- A metadata store based on Redis
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

(module sukkiri-lib
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
        (use s11n) ; FIXME -- using because date->secstring is not working


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL PARAMETERS  ---------------------------------------------

(define *sukkiri-tags-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  UTILITY FUNCTIONS  ---------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

(define (debug-msg . msgs)
  (when (*sukkiri-tags-debug*)
    (with-output-to-port
      (current-error-port)
      (lambda () (apply print msgs)))))

(define (ymd->date y m d)
  (make-date 0 0 0 0 d m y))

(define (ymdhms->date yr mo dt hr mi #!optional (se 0))
  (make-date 0 se mi hr dt mo yr))

(define (get-redis-list key)
  (let* ((len (string->number (car (redis-llen key))))
         (last (number->string (- len 1))))
    (redis-lrange key "0" last)))

(define (get-redis-set key)
  (list->set (redis-smembers key)))

(define (db-result->bool rs)
  (case (car rs)
    ((0) #f)
    ((1) #t)
    (else (eprintf "Result '~A' cannot be converted to a boolean value." rs))))

(define (set-map proc input-set)
  (let ((output-set (make-empty-set)))
    (set-for-each
      (lambda (mem)
        (set-add! output-set (proc mem)))
      input-set)
    output-set))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GENERIC STORAGE PROTOCOL  --------------------------------------

(define (generate-resource-id)
  (let ((base-id (redis-incr "%RESOURCE-ID")))
    (sprintf "%RESOURCE:~X" (car base-id))))

(define (resource-id id)
  (sprintf "%RESOURCE:~A" id))

;(define (resource-type id)
  ;(string->symbol (car (redis-hget (resource-id id) "%TYPE"))))

(define (resource-type id)
  (string->symbol (car (redis-hget id "%TYPE"))))

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

(define (store-list id lst)
  (for-each
    (lambda (elt) (redis-rpush id elt))
    lst))

(define (store-set id set)
  (set-for-each
    (lambda (elt) (redis-sadd id elt))
    set))

(define (store-anonymous-object f obj)
  (let* ((id (generate-anon-id))
         (stored (f id obj)))
    (and stored id)))

; (define (store-anonymous-hash obj)
  ; (store-anonymous-object obj store-hash-table))

(define (store-anonymous-list obj)
  (store-anonymous-object store-list obj))

(define (store-anonymous-set obj)
  (store-anonymous-object store-set obj))

; ;;; ====================================================================
; ;;; --  Retrieval  -----------------------------------------------------

(define (retrieve-anonymous-list id)
  (let ((len (car (redis-llen id))))
    (redis-lrange id "0" (number->string (- len 1)))))

(define (retrieve-anonymous-set id)
  (list->set (redis-smembers id)))

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

(define (string->boolean s)
  (cond
    ((string=? s "T") #t)
    ((string=? s "F") #f)
    (else (eprintf "String '~A' does not represent a boolean." s))))

(define date->secstring
  (lambda (d)
  (with-output-to-string
    (lambda ()
      (serialize d)))))
  ;(o number->string inexact->exact time->seconds date->time))

(define secstring->date
  (lambda (s)
    (with-input-from-string s
      (lambda ()
        (deserialize)))))
  ;(o seconds->date inexact->exact string->number))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  PROPERTY TYPES  ------------------------------------------------

(define-syntax make-pt-primitive
  (syntax-rules ()
    ((_ type-sym to from val)
     (lambda (msg)
       (case msg
         ((type) type-sym)
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
           (base-type 'validator)))
      (else (eprintf "Unrecognized message: ~A" msg)))))

(define (make-prop-list element-type-sym)
  (let ((element-type (hash-table-ref prop-types element-type-sym)))
    (lambda (msg)
      (case msg
        ((type) 'list)
        ((element-type) element-type-sym)
        ((to-string)
         (lambda (lst)
           (store-anonymous-list
             (map (element-type 'to-string) lst))))
        ((from-string)
         (lambda (s)
           (map
             (element-type 'from-string)
             (retrieve-anonymous-list s))))
        ((validator)
         (lambda (lst)
           (let loop ((lst* lst))
             (cond
               ((null? lst*) #t)
               ((not ((element-type 'validator) (car lst*))) #f)
               (else (loop (cdr lst*)))))))
        (else (eprintf "Unrecognized message: ~A" msg))))))

(define (make-prop-set element-type-sym #!optional (tag-set? #f))
  (lambda (msg)
    (let ((element-type (hash-table-ref prop-types element-type-sym)))
      (case msg
        ((type) 'set)
        ((tag-set?) tag-set?)
        ((element-type) element-type-sym)
        ((to-string)
         (lambda (a)
           (store-anonymous-set
             (set-map (element-type 'to-string) a))))
        ((from-string)
         (lambda (s)
           (set-map
             (element-type 'from-string)
             (retrieve-anonymous-set s))))
        ((validator)
         (lambda (a)
           (let ((result #t)
                 (val (element-type 'validator)))
             (set-for-each
               (lambda (m) (set! result (and result (val m))))
               a))))
        (else (eprintf "Unrecognized message: ~A" msg))))))

(define prop-types
  (let ((string-type
          (make-pt-primitive 'string identity identity string?)))
    (alist->hash-table
      (list
        (cons 'string string-type)
        (cons 'char
              (make-pt-primitive
                'char (o list->string list) (o car string->list) char?))
        (cons 'symbol
              (make-pt-primitive
                'symbol symbol->string string->symbol symbol?))
        (cons 'number
              (make-pt-primitive
                'number number->string string->number number?))
        (cons 'boolean
              (make-pt-primitive 
                'boolean boolean->string string->boolean boolean?))
        (cons 'date
              (make-prop-type 'date
                              defined-in: 'srfi-19 to-string: date->secstring
                              from-string: secstring->date validator: date?))
        (cons 'iref
              (make-prop-type 'iref base-type: string-type))
        (cons 'xref
              (make-prop-type 'xref base-type: string-type))
        (cons 'fref
              (make-prop-type 'fref base-type: string-type))))))

(define (make-irregex-validator patt)
  (let ((re (irregex patt)))
    (lambda (str)
      (and (irregex-match re str) #t))))

(define (make-enum-validator choices)
  (lambda (x)
    (member x choices)))

(define (register-prop-type type #!key (base-type #f) (defined-in #f)
                            (to-string #f) (from-string #f) (validator #f)
                            (force! #f))
  (when (and (hash-table-exists? prop-types type)
             (not force!))
    (eprintf "Property type ~A already exists." type))
  (hash-table-set!
    prop-types
    type
    (make-prop-type
      type base-type: base-type defined-in: defined-in
      to-string: to-string from-string: from-string validator: validator)))

(define (register-list-type type-sym element-type #!optional (force! #f))
  (when (and (hash-table-exists? prop-types type-sym)
             (not force!))
    (eprintf "Property type ~A already exists." type-sym))
  (hash-table-set!
    prop-types
    type-sym
    (make-prop-list element-type)))

(define (register-set-type type-sym element-type
                           #!key (force! #f) (tag-set? #f))
  (when (and (hash-table-exists? prop-types type-sym)
             (not force!))
    (eprintf "Property type ~A already exists." type-sym))
  (hash-table-set!
    prop-types
    type-sym
    (make-prop-set element-type tag-set?)))

(define (unregister-prop-type type)
  (hash-table-delete! prop-types type))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  RESOURCE TYPES  ------------------------------------------------

(define resource-types (make-hash-table))
 
; (define-record prop-spec label type has-default? default-value
               ;required? element-type min-size max-size)

(define (make-prop-spec label type #!key (default '(#f)) (required? #t)
                        (element-type #f) (min-size 0) (max-size #f))
  (lambda (msg)
    (case msg
      ((label) label)
      ((type) type)
      ((has-default?) (car default))
      ((default-value) (cadr default))
      ((required?) required?)
      ((element-type) element-type)
      ((min-size) 0)
      ((max-size) #f)
      (else (eprintf "[prop-spec] Unrecognized message: ~A" msg)))))

;; default values need to be lists: the first element indicates whether
;;   or not there is a default, the second is the default value
(define (create-atomic-prop-spec label type #!key
                                 (default '(#f)) (required? #t))
  (make-prop-spec label type default: default required?: required?))

(define (create-struct-prop-spec label type #!key (default '(#f))
                                 (required? #t) (min-size 0) (max-size #f))
  (let* ((type-spec (hash-table-ref prop-types type))
         (elt-type (type-spec 'element-type)))
    (make-prop-spec label type default: default required?: required?
                    element-type: elt-type min-size: min-size
                    max-size: max-size)))

(define (register-resource-type type-name prop-specs)
  (hash-table-set! resource-types type-name prop-specs))

;(define-record resource id type data)

;(define (create-resource id type #!optional (data '()))
;  (make-resource id type (alist->hash-table data)))

(define (xml->register-types)
  #f)

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



; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; --------------------------------------------------------------------
; 
; (define (get-property res-id prop-name)
;   (let* ((res-type (car (redis-hget res-id "%TYPE")))
;          (type-def (hash-table-ref resource-types res-type))
;          ;(prop-spec (type-def prop-name))
;          (prop-spec (alist-ref prop-name type-def string=?))
;          (pt-name (prop-spec-type prop-spec))
;          (pt-def (hash-table-ref prop-types pt-name))
;          (convert (pt-def 'from-string))
;          (required? (prop-spec-required? prop-spec)))
;     (let ((rs (car (redis-hget res-id prop-name))))
;       (cond
;         ((and (null? rs) required?)
;          (eprintf "Required property '~A' is missing." prop-name))
;         ((null? rs)
;          '())
;         (else (convert rs))))))
; 
; (define (set-property res-id prop-name value #!optional (type-def #f))
;   (let* ((res-type (car (redis-hget res-id "%TYPE")))
;          (type-def (hash-table-ref resource-types res-type))
;          (prop-spec (alist-ref prop-name type-def string=?))
;          (pt-name (prop-spec-type prop-spec))
;          (pt-def (hash-table-ref prop-types pt-name))
;          (valid? (pt-def 'validator))
;          (convert (pt-def 'to-string)))
;     (if (valid? value)
;       (redis-hset res-id prop-name (convert value))
;       (error
;         (sprintf
;           "'~A' is not a valid value for property '~A' on object '~A'"
;           value prop-name res-id)))))
; 
; (define (get-resource id)
;   (let* ((type (string->symbol (car (redis-hget id "%TYPE"))))
;          (type-def (hash-table-ref resource-types type))
;          (fields (type-def 'fields))
;          (result (make-hash-table)))
;     (for-each
;       (lambda (fld)
;         (hash-table-set! result fld (get-property id fld type-def)))
;       fields)
;     result))
; 
; (define (set-resource! obj)
;   (let* ((id (resource-id obj))
;          (type (resource-type obj))
;          (type-def (hash-table-ref resource-types type))
;          (data (resource-data obj)))
;     (redis-hset id "%TYPE" (symbol->string type))
;     (hash-table-for-each
;       data
;       (lambda (k v)
;         (set-property id k v type-def)))))
;     
; 
; 
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  DATABASE INDEXES  ----------------------------------------------

(define (index-add! name #!optional (refs '()) #!key (prefix "%HAS-PROP:"))
  (print name)
  (display "REFS: ")
  (pp refs)
  (let ((idx-name (string-append prefix name)))
    (for-each
      (lambda (r) (redis-sadd idx-name r))
      refs)))

;; FIXME: Think we should URLencode the string to handle spaces
(define (tag-index-add! tag #!optional (refs '()))
  (index-add! tag refs prefix: "%TAG:"))

(define (index-delete! name value #!optional (prefix "%HAS-PROP:"))
  (let ((idx-name (string-append prefix name)))
    (redis-srem name value)))

(define (tag-index-delete! name value)
  (index-delete! name value "%TAG:"))

(define (index-exists? name value #!optional (prefix "%HAS-PROP"))
  (db-result->bool (redis-sismember name value)))

(define (tag-index-exists? name value)
  (index-exists? name value "%TAG:"))

(define (get-index name #!optional (prefix "%HAS-PROP:"))
  (let ((idx-name (string-append prefix name)))
    (redis-sismember idx-name)))

(define (get-tag-index name)
  (get-index name "%TAG:"))

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

(define (make-prop-responder res-id prop-sym prop-type)
  (let* ((type-def (hash-table-ref prop-types prop-type))
         (ts (type-def 'to-string))
         (fs (type-def 'from-string))
         (v (type-def 'validator))
         (prop-name (symbol->string prop-sym))
         (tag-set? (and (eqv? (type-def 'type) 'set)
                        (type-def 'tag-set?))))
    (lambda args
      (if (null? args)
        (fs (car (redis-hget res-id prop-name)))
        (let* ((new-val (car args))
               (valid? (v new-val)))
          (if valid? 
            (begin
              (redis-hset res-id prop-name (ts new-val))
              (if tag-set?
                (set-for-each
                  (lambda (m)
                    (tag-index-add! m (list res-id)))
                  new-val)
                (index-add! prop-name (list res-id))))
            (error "Invalid input!")))))))

(define (create-resource-proxy id type)
  (let* ((responders '())
         (prop-specs
           (hash-table-ref resource-types type))
         (add-responder!
           (lambda (p)
             (let* ((pn (car p))
                    (ps (cdr p))
                    (pt (ps 'type)))
               (set! responders
                 (cons
                   (cons pn (make-prop-responder id pn pt))
                   responders))))))
    (for-each add-responder! prop-specs)
    (lambda (arg . args)
      (case arg
        ((id) id)
        ((type) type)
        ((resp) responders) ; just for debugging
        ((add-resp) (for-each add-responder! args))
        (else (apply (alist-ref arg responders) args))))))

(define (create-resource id type #!optional (prop-data '()))
  (redis-hset id "%TYPE" (symbol->string type))
  (let ((prop-specs (hash-table-ref resource-types type))
        (proxy (create-resource-proxy id type)))
    (for-each
      (lambda (ps)
        (let* ((name (car ps))
               (spec (cdr ps))
               (val (alist-ref name prop-data)))
          (if val
            (proxy name val)
            (cond
              ((spec 'has-default?)
               (proxy name (spec 'default-value)))
              ((spec 'required?)
               (eprintf "You must provide a value for '~A'." name))
              (else #f)))))
      prop-specs)
    proxy))

(define (load-resource-proxy id)
  (let ((exists? (db-result->bool (redis-exists id))))
    (unless exists? (eprintf "Resource ~A does not exist." id))
    (let* ((type (resource-type id)))
      (create-resource-proxy id type))))

(define (delete-resource! id)
  ;; First delete index references
  (let* ((props (redis-hkeys id)))
    (for-each
      (lambda (p) (index-delete! p id))
      props)
    (redis-del id)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ====================================================================
;;; --------------------------------------------------------------------
