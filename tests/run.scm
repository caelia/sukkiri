;;; Unit tests for Sukkiri egg

(use (prefix sukkiri-store s:))
(use (prefix sukkiri-data d:))
(use (prefix sukkiri-ajax a:))
(use srfi-1)
(use test)
(use files)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL SUPPORT PROCEDURES  ---------------------------------------

(define (create-test-db)
  (s:%db-file% "test.db")
  (s:create-db "test.db")
  (s:connect))

(define (cleanup-db)
  (s:disconnect)
  (delete-file* "test.db"))

(define reset-comparator
  (let ((default (current-test-comparator)))
    (lambda ()
      (current-test-comparator default))))

;; Tests 2 lists for same members, regardless of order
(define (uequal? l1 l2)
  (and (= (length l1) (length l2))
       (every (lambda (elt) (member elt l2)) l1)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE FUNCTIONS  ----------------------------------------------

;;; ------  Support data  --------------------------------------------------

(define patt-db01-01
  "[-_.a-z]+@[-_.a-z]+")

(define numspec-db01-02
  ;; I don't like that they are floats. Need to fix this.
  '((minval . 0.0) (maxval . 100.0) (step . 5.0) (digits)))

(define terms-db01-03
  '("arachnid" "bonsai" "carcinoma" "dervish" "eloquence" "forensics" "grapefruit"))

(define members-db01-06
  '("simple-email" "some-words" "boolean"))

;;; ========================================================================
;;; ------  Run Tests  -----------------------------------------------------

(test-group "[DB] Database Functions"
  (let ((test-db (create-test-db)))
    (test-group "[DB01] Storing & Retrieving Type Definitions"
      (test
        "DB01.01: String type"
        patt-db01-01
        (begin
          (s:add-string-type test-db "simple-email" patt-db01-01)
          (s:get-string-type test-db "simple-email")))
      (test
        "DB01.02: Number type"
        numspec-db01-02
        (begin
          (s:add-number-type test-db "boozle" minval: 0 maxval: 100 step: 5)
          (s:get-number-type test-db "boozle")))
      (test
        "DB01.03: Vocab type"
        terms-db01-03
        (begin
          (s:add-vocab-type test-db "some-words" terms-db01-03)
          (s:get-vocab-type test-db "some-words")))
      (current-test-comparator uequal?)
      (test
        "DB01.05: Built-in union type 'any'"
        '("boolean" "integer" "float" "string" "date" "time" "period" "nref"
          "rref" "xref" "sref" "simple-email" "boozle" "some-words")
        (s:get-union-type test-db "any"))
      (test
        "DB01.06: User-defined union type"
        members-db01-06
        (begin
          (s:add-union-type test-db "marzipan" members-db01-06)
          (s:get-union-type test-db "marzipan")))
      (reset-comparator)
          )
    (cleanup-db)))
        
;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

