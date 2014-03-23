;;; sukkiri-data.scm -- Data type converters & validators.
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

(module sukkiri-data
        *
        (import scheme chicken)
        (use sukkiri-base)
        (use sukkiri-store)
        (use irregex)
        (use srfi-69)
        (use srfi-19)

 
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
;;; ------------------------------------------------------------------------

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
;;; ------------------------------------------------------------------------

(define (make-union-type-validator members)
  (lambda (x) (member (identify x) members)))

(define (load-union-type-validator db/file type-name)
  (let* ((members (get-union-type db/file type-name))
         (val (make-union-type-validator members)))
    (hash-table-set! validators type-name val)))

(define (load-union-type-validators db/file)
  (let ((union-types (get-union-types db/file)))
    (for-each
      (lambda (t) (load-union-type-validator db/file t))
      union-types)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

