;;; sukkiri-query.scm -- Query processor for Sukkiri
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the BSD
;;;   license. See the accompanying LICENSE file for details.

(module sukkiri-query
        *
        (import scheme chicken)
        (use (prefix sukkiri-base b:))
        (use (prefix sukkiri-store s:))
        (use (prefix sukkiri-data d:))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define (simple-query store query)
  #f)

(define (create-type store name class typespec #!optional description)
  #f)

(define (get-type store name)
  #f)

(define (update-type store name typespec)
  #f)

(define (delete-type store name #!key (allow-orphans #f) (cascade #f))
  #f)

(define (create-struct store type members)
  #f)

(define (update-struct store id members)
  #f)

(define (delete-struct store id)
  #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
)
;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

