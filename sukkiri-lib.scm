;;; sukkiri-lib.scm -- A metadata store based on Redis
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD License. See the accompanying LICENSE file for details.

(module sukkiri-lib
;        *
        (*sukkiri-debug*
         make-prop-type
         make-prop-list
         make-prop-set
         make-irregex-validator
         make-enum-validator
         register-prop-type
         register-list-type
         register-set-type
         unregister-prop-type
         make-prop-spec
         create-atomic-prop-spec
         create-struct-prop-spec
         register-resource-type
         xml->register-types
         index-add!
         index-delete!
         index-exists?
         get-index
         make-prop-responder
         create-resource-proxy
         create-resource
         load-resource-proxy
         delete-resource!)

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
        (use mathh)
        (use sets)
        (use irregex)
        (use yasos)
        (use s11n) ; FIXME -- using because date->secstring is not working


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL PARAMETERS  -------------------------------------------

(define *sukkiri-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

(define (debug-msg . msgs)
  (when (*sukkiri-debug*)
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

(define (null-hook _ _ _) #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GENERIC STORAGE PROTOCOL  ------------------------------------

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

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  PROPERTIES  ------------------------------------------------------

;;; ========================================================================
;;; ----  Basic property type  ---------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate property?) 
(define-operation (prop-type obj))
(define-operation (base-type obj))
(define-operation (get-val obj))
(define-operation (set-val! obj new-val))
(define-operation (->db-string obj))
(define-operation (<-db-string obj))
(define-operation (store obj))
(define-operation (retrieve obj))

;;; ------  Implementation  ------------------------------------------------

;;; make-property should not be used directly in application code. It is
;;;   intended to be a basis for defining more specific types in extension
;;;   code.
(define (make-property ptype base #!key (init-val #f) (valid? (lambda (_) #f))
                       (to-string (lambda (x) x)) (from-string (lambda (s) s)))
  (let ((value init-val)
        (db-result #f))
    (object
      ((property? self) #t)
      ((prop-type self) ptype)
      ((base-type self) base)
      ((get-val self) value)
      ((set-val! self new-val)
       (if (valid? new-val)
         (set! value new-val)
         (error (sprintf "Invalid input for ~A!" ptype))))
      ((store self new-val) (redis-hset res-id prop-name (to-string new-val)))
      ((retrieve self)
       (let ((rs (redis-hget res-id prop-name)))
         (to-string (car rs))))
      (else
        (error (sprintf "Unrecognized message."))))))


;;; ========================================================================
;;; ----  String Type  -----------------------------------------------------

;;; ------  Interface-------------------------------------------------------

(define-predicate string-property?)

;;; ------  Implementation  ------------------------------------------------

(define (make-string-property #!key (init-val #f) (valid? string?))
  (object-with-ancestors ((super (make-property "string" 'string
                                                init-val: init-val
                                                valid?: valid?)))
    ((string-property? self) #t)))


;;; ========================================================================
;;; ----  Boolean Type  ----------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate boolean-property?)

;;; ------  Implementation  ------------------------------------------------

;;; ========================================================================
;;; ----  Regular Expression Type  -----------------------------------------

;;; ------  Interface-------------------------------------------------------

(define-predicate regex-property?)

;;; ------  Implementation  ------------------------------------------------

(define (make-regex-property name pattern #!key (init-val #f))
  (let* ((rx (irregex pattern))
         (valid? (lambda (s) (irregex-match rx s))))
    (object-with-ancestors ((super (make-property name 'string
                                                  init-val: init-val
                                                  valid?: valid?)))
      ((regex-property? self) #t))))


;;; ========================================================================
;;; ----  Numeric Types  ---------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate numeric-property?)
(define-predicate integer-property?)
(define-predicate fixed-prec-property?)
(define-predicate real-property?)
(define-operation (min-val obj))
(define-operation (max-val obj))
(define-operation (anchor-val obj))
(define-operation (interval obj))

;;; ------  Implementation  ------------------------------------------------ 

(define (make-numeric-property type-name
                           #!optional (interval #f)
                           #!key (min-val #f) (max-val #f) (anchor-val #f)
                           (at-interval? (lambda (_) #t)) (init-val #f))
  (let* ((in-range?
           (cond
             ((and min-val max-val)
              (lambda (n) (and (>= n min-val) (<= n max-val))))
             (min-val
               (lambda (n) (>= n min-val)))
             (max-val
               (lambda (n) (<= n max-val)))
             (else
               (lambda (_) #t))))
         (valid?
           (lambda (n) (and (in-range? n) (at-interval? n)))))
    (object-with-ancestors ((super (make-property type-name init-val: init-val
                                                  valid?: valid)))
      ((numeric-property?) #t)
      ((min-val) min-val)
      ((max-val) max-val)
      ((anchor-val) anchor-val)
      ((interval) interval))))

(define (make-integer-property #!optional (interval 1)
                               #!key (min-val #f) (max-val #f) (anchor-val #f)
                               (init-val #f))
  (let ((at-interval?
          (lambda (n)
            (let ((delta
                    (cond
                      (min-val (- n min-val))
                      (max-val (- max-val n))
                      (anchor-val (- n anchor-val))
                      (else n))))
              (= (modulo delta interval) 0)))))
  (object-with-ancestors ((super (make-numeric-property "integer" interval
                                                        at-interval?: at-interval?
                                                        min-val: min-val
                                                        max-val: max-val
                                                        anchor-val: anchor-val
                                                        init-val: init-val)))
    ((integer-property?) #t))))

(define (make-fixed-prec-property decimal-places 
                                  #!optional (interval #f)
                                  #!key (min-val #f) (max-val #f) (anchor-val #f)
                                  (init-val #f))
  (let* ((negative
           (lambda (x) (- (abs x))))
         (interval
           (or interval (expt 10 (negative decimal-places))))
         (at-interval?
           (lambda (x)
             (let ((delta
                     (cond
                       (min-val (- n min-val))
                       (max-val (- max-val n))
                       (anchor-val (- n anchor-val))
                       (else n))))
               (= (fpmod delta interval) 0.0)))))
  (object-with-ancestors ((super (make-numeric-property "fixed-precision" interval
                                                        at-interval?: at-interval?
                                                        min-val: min-val
                                                        max-val: max-val
                                                        anchor-val: anchor-val
                                                        init-val: init-val)))
    ((fixed-prec-property?) #t))))

(define (make-real-property #!key (min-val #f) (max-val #f) (init-val #f))
  (object-with-ancestors ((super (make-numeric-property "real"
                                                        min-val: min-val
                                                        max-val: max-val
                                                        init-val: init-val)))
    ((real-property?) #t)))


;;; ========================================================================
;;; ----  Enumerated Type  -------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate enum-type?)
(define-operation validate obj value)
(define-operation choice->index obj)
(define-operation index->choice obj)
(define-operation choices)
(define-operation enum-size obj)
(define-operation add-choice! obj new-choice)
(define-operation remove-choice! obj choice)

;;; ------  Implementation  ------------------------------------------------

(define (make-enum-type type-name #!optional (choices '()))
  (let ((valid? (lambda (x) (member x choices))))
    (object
      ((validate self value) (valid? value))
      ((choice->index self choice) (list-index (lambda (elt) (equal? elt choice)) choices))
      ((index->choice self index) (list-ref choices index))
      ((choices self) choices)
      ((enum-size self) (length choices))
      ((add-choice! self new-choice) (set! choices (cons new-choice choices)))
      ((remove-choice! self choice) (delete! choice choices)))))


;;; ========================================================================
;;; ----  LIST PROPERTY  ---------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate list-property?)

;;; ------  Implementation  ------------------------------------------------

;;; ========================================================================
;;; ------------------------------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

;;; ------  Implementation  ------------------------------------------------


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

(define (make-prop-type type #!key (base-type #f) (defined-in #f)
                        (to-string #f) (from-string #f) (validator #f)
                        (pre-hook #f) (post-hook #f))
  (let ((post-hook
          (or post-hook
              (lambda (res-id prop-name _)
                (index-add! prop-name (list res-id))))))

    (lambda (msg)
      (case msg
        ((type) type)
        ((pre-hook) pre-hook)
        ((post-hook) post-hook)
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
        (else (eprintf "Unrecognized message: ~A" msg))))))

(define (make-prop-list element-type-sym
                        #!key (pre-hook #f) (post-hook #f))
  (let ((element-type (hash-table-ref prop-types element-type-sym)))
    (lambda (msg)
      (case msg
        ((type) 'list)
        ((pre-hook) pre-hook)
        ((post-hook) post-hook)
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

(define (make-prop-set element-type-sym
                       #!key (pre-hook #f) (post-hook #f))
  (lambda (msg)
    (let ((element-type (hash-table-ref prop-types element-type-sym)))
      (case msg
        ((type) 'set)
        ((pre-hook) pre-hook)
        ((post-hook) post-hook)
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
                            (pre-hook #f) (post-hook #f) (force! #f))
  (when (and (hash-table-exists? prop-types type)
             (not force!))
    (eprintf "Property type ~A already exists." type))
  (hash-table-set!
    prop-types
    type
    (make-prop-type
      type base-type: base-type defined-in: defined-in
      to-string: to-string from-string: from-string validator: validator
      pre-hook: pre-hook post-hook: post-hook)))

(define (register-list-type type-sym element-type
                            #!key (force! #f) (pre-hook #f) (post-hook #f))
  (when (and (hash-table-exists? prop-types type-sym)
             (not force!))
    (eprintf "Property type ~A already exists." type-sym))
  (hash-table-set!
    prop-types
    type-sym
    (make-prop-list element-type pre-hook: pre-hook post-hook: post-hook)))

(define (register-set-type type-sym element-type
                           #!key (force! #f) (pre-hook #f) (post-hook #f))
  (when (and (hash-table-exists? prop-types type-sym)
             (not force!))
    (eprintf "Property type ~A already exists." type-sym))
  (hash-table-set!
    prop-types
    type-sym
    (make-prop-set element-type pre-hook: pre-hook post-hook: post-hook)))

(define (unregister-prop-type type)
  (hash-table-delete! prop-types type))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RESOURCE TYPES  ----------------------------------------------

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

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE INDEXES  --------------------------------------------

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



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  PROXY OBJECTS  -----------------------------------------------

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

(define proxies (make-hash-table))

(define (make-prop-responder res-id prop-sym prop-type)
  (let* ((type-def (hash-table-ref prop-types prop-type))
         (ts (type-def 'to-string))
         (fs (type-def 'from-string))
         (v (type-def 'validator))
         (prop-name (symbol->string prop-sym))
         (pre-hook (or (type-def 'pre-hook)
                       null-hook))
         (post-hook (or (type-def 'post-hook)
                       null-hook)))
    (lambda args
      (if (null? args)
        (fs (car (redis-hget res-id prop-name)))
        (let* ((new-val (car args))
               (valid? (v new-val)))
          (if valid? 
            (begin
              (pre-hook res-id prop-name (ts new-val))
              (redis-hset res-id prop-name (ts new-val))
              (post-hook res-id prop-name (ts new-val)))
            (error "Invalid input!")))))))

(define (create-resource-proxy id type)
  (let* ((responders '())
         (delete-hooks '())
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
        ((add-resp!)
         (for-each add-responder! args))
        ((add-delete-hook!)
         (set! delete-hooks (cons (car args) delete-hooks)))
        ((run-delete-hooks)
         (for-each eval delete-hooks))
        (else
          (apply (alist-ref arg responders) args))))))

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
    (let* ((type (resource-type id))
           (proxy (create-resource-proxy id type)))
      (hash-table-set! proxies id proxy)
      proxy)))

(define (get-resource-proxy id)
  (let ((exists (hash-table-exists? proxies id)))
    (if exists
      (hash-table-ref proxies id)
      (load-resource-proxy id))))

(define (delete-resource! id)
  ;; First delete index references
  (let* ((props (redis-hkeys id))
         (proxy (get-resource-proxy id))
    (for-each
      (lambda (p) (index-delete! p id))
      props)
    (proxy 'run-delete-hooks)
    (redis-del id)
    (hash-table-delete proxies id)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------
;;; ------  Interface  -----------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Implementation  ------------------------------------------------

