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

        ;(import (prefix sukkiri-utils util:))
        ;(import sukkiri-utils)
        (reexport sukkiri-utils)
        (import (prefix sukkiri-db db:))

        (use numbers)
        (use mathh)
        (use sets)
        (use irregex)
        (use yasos)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

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
(define-operation (constraints obj))
(define-operation (storage-proc obj))
(define-operation (retrieval-proc obj))
(define-operation (deletion-proc obj))

;;; ------  Implementation  ------------------------------------------------

(define (make-proptype base-type valid? #!optional ->dbformat dbformat->)
  (let ((->dbformat
          (or ->dbformat (db:converter-from base-type)))
        (dbformat->
          (or dbformat-> (db:converter-to base-type))))
    (object
      ((proptype? self) #t) 
      ((base-type self) base-type)
      ((structure self) #f)
      ((constraints self) #f)
      ((storage-proc self)
       (lambda (res-id prop-name new-val)
         (if (valid? new-val)
           (db:store-property res-id prop-name (->dbformat new-val))
           (eprintf "~A is not a legal value for ~A:~A" new-val res-id prop-name))))
      ((retrieval-proc self)
       (lambda (res-id prop-name)
         (dbformat-> (db:retrieve-property res-id prop-name))))
      ((deletion-proc self)
       (lambda (res-id prop-name)
         (db:delete-property res-id prop-name))))))

;;; ========================================================================
;;; ----  Regular Expression Type  -----------------------------------------

;;; ------  Interface-------------------------------------------------------

(define-predicate regex-proptype?)

;;; ------  Implementation  ------------------------------------------------

(define (make-regex-proptype pattern)
  (let* ((rx (irregex pattern))
         (valid? (lambda (s) (irregex-match rx s))))
    (object-with-ancestors
      ((super (make-proptype 'string valid?: valid?)))
      ((regex-property? self) #t))))


;;; ========================================================================
;;; ----  Numeric Types  ---------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate numeric-proptype?)
(define-predicate integer-proptype?)
(define-predicate fixed-prec-proptype?)
(define-predicate real-proptype?)
(define-operation (min-val obj))
(define-operation (max-val obj))
(define-operation (anchor-val obj))
(define-operation (interval obj))

;;; ------  Implementation  ------------------------------------------------ 

(define (make-numeric-proptype #!optional (base-type 'number) (interval #f)
                               #!key (min-val #f) (max-val #f) (anchor-val #f)
                               (at-interval? (lambda (_) #t)))
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
    (object-with-ancestors ((super (make-proptype type-name init-val: init-val
                                                  valid?: valid)))
      ((numeric-proptype?) #t)
      ((min-val) min-val)
      ((max-val) max-val)
      ((anchor-val) anchor-val)
      ((interval) interval))))

(define (make-integer-proptype #!optional (interval 1)
                               #!key (min-val #f) (max-val #f) (anchor-val #f))
  (let ((at-interval?
          (lambda (n)
            (let ((delta
                    (cond
                      (min-val (- n min-val))
                      (max-val (- max-val n))
                      (anchor-val (- n anchor-val))
                      (else n))))
              (= (modulo delta interval) 0)))))
  (object-with-ancestors ((super (make-numeric-proptype 'integer interval
                                                        at-interval?: at-interval?
                                                        min-val: min-val
                                                        max-val: max-val
                                                        anchor-val: anchor-val)))
    ((integer-proptype?) #t))))

(define (make-fixed-prec-proptype decimal-places #!optional (interval #f)
                                  #!key (min-val #f) (max-val #f) (anchor-val #f))
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
  (object-with-ancestors ((super (make-numeric-proptype 'float interval
                                                        at-interval?: at-interval?
                                                        min-val: min-val
                                                        max-val: max-val
                                                        anchor-val: anchor-val)))
    ((fixed-prec-proptype?) #t))))

(define (make-real-proptype #!key (min-val #f) (max-val #f))
  (object-with-ancestors ((super (make-numeric-proptype 'float
                                                        min-val: min-val
                                                        max-val: max-val)))
    ((real-proptype?) #t)))


;;; ========================================================================
;;; ----  Enumerated Type  -------------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate enum-proptype?)
(define-operation validate obj value)
(define-operation choice->index obj)
(define-operation index->choice obj)
(define-operation choices)
(define-operation enum-size obj)
(define-operation add-choice! obj new-choice)
(define-operation remove-choice! obj choice)

;;; ------  Implementation  ------------------------------------------------

(define (make-enum-proptype #!optional (base-type 'string) (choices '()))
  (let ((valid? (lambda (x) (member x choices))))
    (object-with-ancestors ((super (make-proptype base-type valid?: valid?)))
      ((enum-proptype? self) #t)
      ((choice->index self choice) (list-index (lambda (elt) (equal? elt choice)) choices))
      ((index->choice self index) (list-ref choices index))
      ((choices self) choices)
      ((enum-size self) (length choices))
      ((add-choice! self new-choice) (set! choices (cons new-choice choices)))
      ((remove-choice! self choice) (delete! choice choices)))))


;;; ========================================================================
;;; ----  PROPERTY TYPE REGISTRY  ------------------------------------------

(define proptypes
  (alist->hash-table
    (list
      (cons "string" (make-proptype 'string string?))
      (cons "char" (make-proptype 'char char?))
      (cons "symbol" (make-proptype 'symbol symbol?))
      (cons "boolean" (make-proptype 'boolean boolean?))
      (cons "integer" (make-proptype 'integer fixnum?))
      (cons "float" (make-proptype 'float flonum?))
      (cons "number" (make-proptype 'number number?))
      (cons "iref" (make-proptype 'string string?)))))

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
      ptnames)))

(define (get-proptype name)
  (hash-table-ref proptypes name))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  PROPERTIES  ------------------------------------------------------

;;; ----  Basic Property Object  -------------------------------------------

;;; ------  Interface  -----------------------------------------------------

(define-predicate property?)
(define-predicate list-property?)
(define-predicate set-property?)
(define-operation (p-update! obj new-val))
(define-operation (p-reset! obj))
(define-operation (p-get obj))
(define-operation (p-delete! obj))

;;; ------  Implementation  ------------------------------------------------

(define (initialize-property (res-id prop-name init default store))
  (let ((init (or init default)))
    (if init
      (store res-id prop-name init)
      (db:store-undef res-id prop-name))))

(define (make-property res-id prop-name ptype-name #!optional (init #f)
                       #!key (default #f) (required #t) (initialize #t))
  (let ((prop-type (get-proptype ptype-name)))
    (when initialize
      (initialize-property res-id prop-name init default (storage-proc prop-type)))
    (object
      ((p-update! self new-val)
       ((storage-proc (prop-type)) res-id prop-name new-val))
      ((p-reset! self)
       (when default
         ((storage-proc (prop-type)) res-id prop-name default)))
      ((p-get self)
       ((retrieval-proc (prop-type)) res-id prop-name))
      ((p-delete! self)
       ((deletion-proc (prop-type)) res-id prop-name)))))

(define (make-list-property res-id prop-name ptype-name #!optional (init #f)
                            #!key (default #f) (min-occurs 1) (max-occurs '*))
                            

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RESOURCE SCHEMAS  ------------------------------------------------

;;; ========================================================================
;;; ----  RESOURCE SCHEMA REGISTRY  ----------------------------------------

(define (schemata (make-hash-table)) 

(define (register-schema res-type schema)
  (hash-table-set! schemata res-type schema))

(define (create-schema res-type schema)
  (db:store-schema res-type schema)
  (register-schema res-type schema))

(define (load-schema res-type)
  (let ((schema (db:retrieve-schema res-type)))
    (register-schema res-type schema)))

(define (load-schemas)
  (let ((res-types (db:list-restypes)))
    (for-each
      (lambda (rt) (load-schema rt))
      res-types)))

(define (get-schema res-type)
  (hash-table-ref schemata res-type))
  
;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RESOURCES  -------------------------------------------------------

;;; ------  Interface  -----------------------------------------------------
   
(define-predicate resource?)
(define-operation (r-get obj prop-name))
(define-operation (r-set! obj prop-name new-val))
(define-operation (r-unset! obj prop-name))
(define-operation (r-delete! obj))

;;; ------  Implementation  ------------------------------------------------

(define (make-resource id type #!optional (init-values '()) (schema #f)))
  (when (and (eqv? type %AD-HOC:) (not schema))
    (error "You must supply a schema for an ad-hoc resource."))
  (let ((schema (or schema (get-schema type))))
    (for-each
      (lambda (prop-spec)
        (let* ((prop-name (car prop-spec))
               (prop-type (cadr prop-spec))
               (params (cddr prop-spec))
               (default (alist-ref 'default params))
               (min-occurs (or (alist-ref 'min-occurs) 1))
               (max-occurs (or (alist-ref 'max-occurs) 1))
               (structure (and (> max-occurs 1) (alist-ref 'structure params)))
               (init-val (alist-ref prop-name init-values)))
          (
          (object
            ((r-get) #t)
            ((r-set!) #t)
            ((r-unset!) #t)
            ((r-delete! #t))))))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  ADMIN & INITIALIZATION  ------------------------------------------

(define (initialize app-name #!optional (redis-session #t))
  (when redis-session
    (open-db-session app-name))
  (load-proptypes)
  (load-schemas))

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

