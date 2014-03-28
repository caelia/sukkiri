;;; sukkiri-data.scm -- Data type converters & validators.
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

(module sukkiri-data
        *
        (import scheme chicken)
        (import data-structures)
        (use sukkiri-base)
        (use sukkiri-store)
        (use irregex)
        (use srfi-1)
        (use srfi-69)
        (use srfi-19)
        (use srfi-19-period)

 
;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USER TYPE VALIDATION  --------------------------------------------

(define validators (make-hash-table))

;;; ========================================================================
;;; ------  Set up primitive type validators  ------------------------------

(define (setup-primitive-validators #!optional (custom-validators '()))
  (hash-table-set! validators "integer"
                   (or (alist-ref 'integer custom-validators)
                       integer?))
  (hash-table-set! validators "float"
                   (or (alist-ref 'float custom-validators)
                       flonum?))
  (hash-table-set! validators "boolean"
                   (or (alist-ref 'boolean custom-validators)
                       boolean?))
  (hash-table-set! validators "string"
                   (or (alist-ref 'string custom-validators)
                       string?))
  (hash-table-set! validators "date"
                   (or (alist-ref 'date custom-validators)
                       date?))
  (hash-table-set! validators "time"
                   (or (alist-ref 'time custom-validators)
                       time?))
  (hash-table-set! validators "period"
                   (or (alist-ref 'period custom-validators)
                       time-period?))
  (hash-table-set! validators "datetime"
                   (or (alist-ref 'datetime custom-validators)
                       (lambda (x)
                         (and (list? x)
                              (= (length x) 2)
                              (date? (car x))
                              (time? (cadr x))))))
  (hash-table-set! validators "nref"
                   (or (alist-ref 'nref custom-validators)
                       string?))
  (hash-table-set! validators "rref"
                   (or (alist-ref 'rref custom-validators)
                       string?))
  (hash-table-set! validators "xref"
                   (or (alist-ref 'xref custom-validators)
                       string?)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USER TYPE VALIDATORS  --------------------------------------------

;;; ------  String Types  --------------------------------------------------

(define (make-string-type-validator pattern)
  (let ((rx (irregex pattern)))
    (lambda (s) (irregex-match rx s))))

(define (load-string-type-validator db/file type-name)
  (let* ((pattern (get-string-type db/file type-name))
         (val (make-string-type-validator pattern)))
    (hash-table-set! validators type-name val)))

(define (load-string-type-validators db/file)
  (let ((string-types (get-string-types db/file)))
    (for-each
      (lambda (t) (load-string-type-validator db/file t))
      string-types)))

;;; ========================================================================
;;; ------  Number Types  --------------------------------------------------

(define (make-number-type-validator typespec)
  (let* ((minval (alist-ref 'minval typespec))
         (maxval (alist-ref 'maxval typespec))
         (step (alist-ref 'step typespec)))
    (lambda (x)
      (and (or (null? minval)
               (>= x minval))
           (or (null? maxval)
               (<= x maxval))
           (or (null? step)
               (integer?
                  (/ (or (and (null? minval) x)
                         (- x minval)) step)))))))

(define (load-number-type-validator db/file type-name)
  (let* ((typespec (get-number-type db/file type-name))
         (val (make-number-type-validator typespec)))
    (hash-table-set! validators type-name val)))

(define (load-number-type-validators db/file)
  (let ((number-types (get-number-types db/file)))
    (for-each
      (lambda (t) (load-number-type-validator db/file t))
      number-types)))

;;; ========================================================================
;;; ------  Vocabulary Types  ----------------------------------------------

(define (make-vocab-type-validator terms)
  (lambda (x) (member x terms)))

(define (load-vocab-type-validator db/file type-name)
  (let* ((terms (get-vocab-type db/file type-name))
         (val (make-vocab-type-validator terms)))
    (hash-table-set! validators type-name val)))

(define (load-vocab-type-validators db/file)
  (let ((vocab-types (get-vocab-types db/file)))
    (for-each
      (lambda (t) (load-vocab-type-validator db/file t))
      vocab-types)))

;;; ========================================================================
;;; ------  Struct Types  --------------------------------------------------

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
    (every (lambda (mem) (member (alist-ref 'rel-name mem) known-rel-names)) value)))

(define (make-struct-type-validator typespec)
  (let ((extensible (car typespec))
        (memspecs (cadr typespec)))
    (lambda (x)
      (and (every (lambda (ms) (validate-struct-member ms x)) memspecs)
           (or extensible
               (no-unspecified-members? memspecs x))))))

(define (load-struct-type-validator db/file type-name)
  (let* ((typespec (get-struct-type db/file type-name))
         (val (make-struct-type-validator typespec)))
    (hash-table-set! validators type-name val)))

(define (load-struct-type-validators db/file)
  (let ((struct-types (get-struct-types db/file)))
    (for-each
      (lambda (t) (load-struct-type-validator db/file t))
      struct-types)))

;;; ========================================================================
;;; ------  Union Types  ---------------------------------------------------

(define (make-union-type-validator members)
  (lambda (x)
    (any
      (lambda (memtype)
        (validate memtype x))
      members)))

(define (load-union-type-validator db/file type-name)
  (let* ((members (get-union-type db/file type-name))
         (val (make-union-type-validator members)))
    (hash-table-set! validators type-name val)))

(define (load-union-type-validators db/file)
  (let ((union-types (get-union-types db/file)))
    (for-each
      (lambda (t) (load-union-type-validator db/file t))
      union-types)))

;;; ========================================================================
;;; ------  Generic Validation  --------------------------------------------

(define (validate type value)
  (or (equal? type "*")
      (and (hash-table-exists? validators type)
           (let ((validator (hash-table-ref validators type)))
             (validator value)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE STORAGE & RETRIEVAL  ------------------------------------

(define (store-object obj)
  (let-values (((id-pair rest)
                (partition (lambda (elt) (eqv? (car elt) '%ID)) obj)))
    (let-values (((type-pair rest)
                  (partition (lambda (elt) (eqv? (car elt) '%TYPE)) rest)))
      (let ((id (cdr id-pair))
            (type (cdr type-pair)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

