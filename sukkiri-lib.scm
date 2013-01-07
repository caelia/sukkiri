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
        (import sukkiri-db)

        (use srfi-19)
        (use numbers)
        (use mathh)
        (use sets)
        (use irregex)
        (use yasos)
        (use s11n) ; FIXME -- using because date->secstring is not working


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL PARAMETERS  -----------------------------------------------

(define *sukkiri-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

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
;;; ----  PROPERTY TYPES  --------------------------------------------------

;;; ========================================================================
;;; ----  Basic property type  ---------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate proptype?) 
(define-operation (base-type obj))
(define-operation (structure obj))
(define-operation (storage-proc obj))
(define-operation (retrieval-proc obj))
(define-operation (deletion-proc obj))

;;; ------  Implementation  ------------------------------------------------

;;; FIXME: this is not right--still need to figure out how we interface w/
;;;   the DB module
(define (make-proptype base valid? ->dbformat dbformat->)
  (object
    ((proptype? self) #t) 
    ((base-type self) base)
    ((structure self) #f)
    ((storage-proc self)
     (lambda (res-id prop-name new-val)
       (db:store-property res-id prop-name (->dbformat new-val))))
    ((retrieval-proc self)
     (lambda (res-id prop-name)
       (dbformat-> (db:retrieve-property res-id prop-name))))
    ((deletion-proc self)
     (lambda (res-id prop-name)
       (db:delete-property res-id prop-name)))))

;;; ========================================================================
;;; ----  Property Type Registry  ------------------------------------------

(define proptypes (make-hash-table))

(define (register-proptype name ptype-obj)
  (hash-table-set! proptypes name ptype-obj))

(define (create-proptype name base valid? ->dbformat dbformat-> #!optional (store #t))
  (let ((ptype-obj (make-proptype base valid? ->dbformat dbformat->)))
    (when store (db:store-proptype name ptype-obj))
    (register-proptype name ptype-obj)))

(define (load-proptype name)
  (let ((ptype-obj (db:retrieve-proptype name)))
    (register-proptype name ptype-obj)))

(define (load-proptypes)
  (let ((ptnames (db:list-proptypes)))
    (for-each
      (lambda (ptn) (load-proptype ptn))
      (ptnames))))

(define (get-proptype name)
  (hash-table-ref proptypes name))

;;; ========================================================================
;;; ----  Basic Property Object  -------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate property?)
(define-operation (update obj new-val))
(define-operation (reset obj))
(define-operation (get obj))
(define-operation (delete obj))

;;; ------  Implementation  ------------------------------------------------

(define (make-property res-id prop-name ptype-name #!optional (init %UNDEFINED:)
                       #!key (required #t) (default %UNDEFINED:))
  (let ((prop-type (lambda () (get-proptype ptype-name))))
    (if (eqv? init %UNDEFINED:)
       (db:store-undef res-id prop-name)
       ((storage-proc (prop-type)) res-id prop-name init))
    (object
      ((property? self) #t)
      ((update self new-val)
       ((storage-proc (prop-type)) res-id prop-name new-val))
      ((reset self)
       (unless (eqv? default %UNDEFINED:)
         ((storage-proc (prop-type)) res-id prop-name default)))
      ((get self)
       ((retrieval-proc (prop-type)) res-id prop-name))
      ((delete self)
       ((deletion-proc (prop-type)) res-id prop-name)))))


;;; ========================================================================
;;; ----  String Type  -----------------------------------------------------

;;; ------  Interface-------------------------------------------------------

(define-predicate string-property?)

;;; ------  Implementation  ------------------------------------------------

(define (make-string-property res-id prop-name #!key (init-val #f) (valid? string?))
  (object-with-ancestors ((super (make-property "string" 'string res-id prop-name
                                                init-val: init-val valid?: valid?)))
    ((string-property? self) #t)))


;;; ========================================================================
;;; ----  Boolean Type  ----------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate boolean-property?)

;;; ------  Implementation  ------------------------------------------------

(define (make-boolean-property res-id prop-name #!key (init-val #f) (valid? boolean?))
  (let ((ts
          (lambda (b)
              (if b "True" "False")))
        (fs
          (lambda (s) 
            (cond
              ((string=? s "True") #t)
              ((string=? s "False") #f)))))
  (object-with-ancestors ((super (make-property "boolean" 'boolean res-id prop-name
                                                init-val: init-val valid?: valid?
                                                to-string: ts from-string fs)))
    ((boolean-property? self) #t)))

;;; ========================================================================
;;; ----  Regular Expression Type  -----------------------------------------

;;; ------  Interface-------------------------------------------------------

(define-predicate regex-property?)

;;; ------  Implementation  ------------------------------------------------

(define (make-regex-property name pattern res-id prop-name #!key (init-val #f))
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
;;; ----  RESOURCE TYPES  --------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate prop-spec?)
(define-operation label obj)
(define-operation type obj)
(define-operation has-default? obj)
(define-operation default-value obj)
(define-operation required? obj)
(define-operation structure obj)
(define-operation elt-type obj)
(define-operation min-size obj)
(define-operation max-size obj)
   
;;; ------  Implementation  ------------------------------------------------

(define (make-prop-spec label type #!key (default '(#f)) (required? #t)
                        (min-size 0) (max-size #f))
  (object
    ((prop-spec? self) #t)
    ((label self) label)
    ((type self) type)
    ((has-default? self) (car default))
    ((default-value self) (cadr default))
    ((required? self) required?)
    ((structure) (and (list? type) (car type)))
    ((elt-type) (and (list? type) (cadr type)))
    ((min-size self) min-size)
    ((max-size self) max-size)))

;;; #### old code ##########################################################
 
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

(define (xml->register-types)
  #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  PROXY OBJECTS  ---------------------------------------------------

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

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

;;; ========================================================================
;;; ------  Interface  -----------------------------------------------------
   
;;; ------  Implementation  ------------------------------------------------

