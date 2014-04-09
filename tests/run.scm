;;; Unit tests for Sukkiri egg

(use (prefix sukkiri-store s:))
(use (prefix sukkiri-data d:))
(use (prefix sukkiri-ajax a:))
(use srfi-1)
(use srfi-19)
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
;;; ----  SUPPORT DATA  ----------------------------------------------------

(define patt01
  "[-_.a-z]+@[-_.a-z]+")

(define numspec01
  ;; I don't like that they are floats. Need to fix this.
  '((minval . 0.0) (maxval . 100.0) (step . 5.0) (digits)))

(define terms01
  '("arachnid" "bonsai" "carcinoma" "dervish" "eloquence" "forensics" "grapefruit"))

(define struct01
  '(#f ((label "one" "string") (address "one" "string"))))

(define struct02
  '(#f ((%TYPE "sref" "email-address") (label "one" "string") (address "one" "string"))))

(define union-members01
  '("simple-email" "some-words" "boolean"))

(define date01
  (make-date 0 21 45 3 27 2 2014))

(define time01
  (date->time date01))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE FUNCTIONS  ----------------------------------------------

(define test-db (create-test-db))

(test-group "[DB] Database Functions"
  (test-group "[DB01] Storing & Retrieving Type Definitions"
    (test
      "DB01.01: String type"
      patt01
      (begin
        (s:add-string-type test-db "simple-email" patt01)
        (s:get-string-type test-db "simple-email")))
    (test
      "DB01.02: Number type"
      numspec01
      (begin
        (s:add-number-type test-db "boozle" minval: 0 maxval: 100 step: 5)
        (s:get-number-type test-db "boozle")))
    (test
      "DB01.03: Vocab type"
      terms01
      (begin
        (s:add-vocab-type test-db "some-words" terms01)
        (s:get-vocab-type test-db "some-words")))
    (test
      "DB01.04: Struct type"
      struct01
      (begin
        (s:add-struct-type test-db "email-address" extensible: #f
                           members: '((label "one" "string") (address "one" "string")))
        (s:get-struct-type test-db "email-address")))
    (current-test-comparator uequal?)
    (test
      "DB01.05: Built-in union type 'any'"
      '("boolean" "integer" "float" "string" "date" "time" "period" "nref"
        "rref" "xref" "sref" "simple-email" "boozle" "some-words" "email-address")
      (s:get-union-type test-db "any"))
    (test
      "DB01.06: User-defined union type"
      union-members01
      (begin
        (s:add-union-type test-db "marzipan" union-members01)
        (s:get-union-type test-db "marzipan")))
    (reset-comparator)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  VALIDATION  ------------------------------------------------------

(test-group "[VN] Validation"
  (test-group "[VN01] Validating Primitive Types"
    (d:setup-primitive-validators)
    (test
      "VN01.01: boolean [#t - valid]"
      '("boolean" . #t)
      (d:validate "boolean" #t))
    (test
      "VN01.02: boolean [#f - valid]"
      '("boolean" . #f)
      (d:validate "boolean" #f))
    (test
      "VN01.03: boolean [42 - invalid]"
      #f
      (d:validate "boolean" 42))
    (test
      "VN01.04: boolean [\"snorkel\" - invalid]"
      #f
      (d:validate "boolean" "snorkel"))
    (test
      "VN01.05: boolean ['(a b c d e) - invalid]"
      #f
      (d:validate "boolean" '(a b c d e)))
    (test
      "VN01.06: integer [1000 - valid]"
      '("integer" . 1000)
      (d:validate "integer" 1000))
    (test
      "VN01.07: integer [522.3 - invalid]"
      #f
      (d:validate "integer" 522.3))
    (test
      "VN01.08: integer [#t - invalid]"
      #f
      (d:validate "integer" #t))
    (test
      "VN01.09: integer [\"hydrangea\" - invalid]"
      #f
      (d:validate "integer" "hydrangea"))
    (test
      "VN01.10: integer ['((a . 4) (b . #f) (c . junk)) - invalid]"
      #f
      (d:validate "integer" '((a . 4) (b . #f) (c . junk))))
    (test
      "VN01.11: float [-17.33 - valid]"
      '("float" . -17.33)
      (d:validate "float" -17.33))
    (test
      "VN01.12: float [507.0 - valid]"
      '("float" . 507.0)
      (d:validate "float" 507.0))
    (test
      "VN01.13: float [2000 - invalid]"
      #f
      (d:validate "float" 2000))
    (test
      "VN01.14: float [\"barley\" - invalid]"
      #f
      (d:validate "float" "barley"))
    (test
      "VN01.15: float ['() - invalid]"
      #f
      (d:validate "float" '()))
    (test
      "VN01.16: string [\"All mimsy were the borogoves.\" - valid]"
      '("string" . "All mimsy were the borogoves.")
      (d:validate "string" "All mimsy were the borogoves."))
    (test
      "VN01.17: string ['(1000 2000 3000) - invalid]"
      #f
      (d:validate "string" '(1000 2000 3000)))
    (test
      "VN01.18: string [#t - invalid]"
      #f
      (d:validate "string" #f))
    (test
      "VN01.19: string [42 - invalid]"
      #f
      (d:validate "string" 42))
    (test
      "VN01.20: string [#\\C - invalid]"
      #f
      (d:validate "string" #\C))
    (test
      "VN01.21: date [<date01> - valid]"
      `("date" . ,date01)
      (d:validate "date" date01))
    (test
      "VN01.22: date [<time01> - invalid]"
      #f
      (d:validate "date" time01))
    (test
      "VN01.23: date [2000 - invalid]"
      #f
      (d:validate "date" 2000))
    (test
      "VN01.24: date [\"barley\" - invalid]"
      #f
      (d:validate "date" "barley"))
    (test
      "VN01.25: date ['() - invalid]"
      #f
      (d:validate "date" '()))
    (test
      "VN01.26: time [<time01> - valid]"
      `("time" . ,time01)
      (d:validate "time" time01))
    (test
      "VN01.27: time [<date01> - invalid]"
      #f
      (d:validate "time" date01))
    (test
      "VN01.28: time [2000 - invalid]"
      #f
      (d:validate "time" 2000))
    (test
      "VN01.29: time [\"barley\" - invalid]"
      #f
      (d:validate "time" "barley"))
    (test
      "VN01.30: time ['() - invalid]"
      #f
      (d:validate "time" '())))
  (test-group "[VN02] Validating User-defined Atomic Types"
    (d:load-string-type-validator test-db "simple-email")
    (test
      "VN02.01: String Type [valid]"
      '("simple-email" . "mickey-mouse@disneyland.com")
      (d:validate "simple-email" "mickey-mouse@disneyland.com"))
    (test
      "VN02.02: String Type [invalid]"
      #f
      (d:validate "simple-email" "frognorz:231"))
    (test
      "VN02.03: String Type [invalid]"
      #f
      (d:validate "simple-email" "http://allagash.bonzo.com/"))
    (d:load-number-type-validator test-db "boozle")
    (test
      "VN02.04: Number Type [35 - valid]"
      '("boozle" . 35)
      (d:validate "boozle" 35))
    (test
      "VN02.05: Number Type [35.0 - valid]"
      '("boozle" . 35.0)
      (d:validate "boozle" 35.0))
    (test
      "VN02.06: Number Type [0 - valid]"
      '("boozle" . 0)
      (d:validate "boozle" 0))
    (test
      "VN02.07: Number Type [0.0 - valid]"
      '("boozle" . 0.0)
      (d:validate "boozle" 0.0))
    (test
      "VN02.08: Number Type [100 - valid]"
      '("boozle" . 100)
      (d:validate "boozle" 100))
    (test
      "VN02.09: Number Type [100.0 - valid]"
      '("boozle" . 100.0)
      (d:validate "boozle" 100.0))
    (test
      "VN02.10: Number Type [100.003 - invalid]"
      #f
      (d:validate "boozle" 100.003))
    (test
      "VN02.11: Number Type [-37 - invalid]"
      #f
      (d:validate "boozle" -37))
    (d:load-vocab-type-validator test-db "some-words")
    (test
      "VN02.12: Vocab type [\"dervish\" - valid]"
      '("some-words" . "dervish")
      (d:validate "some-words" "dervish"))
    (test
      "VN02.13: Vocab type [\"mycelium\" - invalid]"
      #f
      (d:validate "some-words" "mycelium"))
    (test
      "VN02.14: Vocab type [29 - invalid]"
      #f
      (d:validate "some-words" 29))
    (d:load-union-type-validator test-db "marzipan")
    (test
      "VN02.15: Union type [\"dervish\" - valid]"
      '("some-words" . "dervish")
      (d:validate "marzipan" "dervish"))
    (test
      "VN02.16: Union type [\"wilma-flintstone@bedrock.net\" - valid]"
      '("simple-email" . "wilma-flintstone@bedrock.net")
      (d:validate "marzipan" "wilma-flintstone@bedrock.net"))
    (test
      "VN02.17: Union type [#f - valid]"
      '("boolean" . #f)
      (d:validate "marzipan" #f))
    (test
      "VN02.18: Union type [\"mycelium\" - invalid]"
      #f
      (d:validate "marzipan" "mycelium"))
    (test
      "VN02.19: Union type ['(a b c) - invalid]"
      #f
      (d:validate "marzipan" '(a b c)))
    (test
      "VN02.18: Union type [1014 - invalid]"
      #f
      (d:validate "marzipan" 1014))
              )
  (test-group "[VN03] Validating Struct Types"
              ))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

(cleanup-db)

(test-exit)
