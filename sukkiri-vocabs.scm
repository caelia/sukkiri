;;; sukkiri-vocabs.scm -- Support for controlled vocabularies in Sukkiri.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.

(module sukkiri-vocabs
        *
        (import scheme)
        (import chicken)
        
        (use sukkiri-lib)

(define (vocab-index-add! vocab-name terms)
  (index-add! vocab-name terms prefix: "%VOCAB:"))

(define (term-index-add! vocab term refs)
  (let ((pfx (string-append "%VOCAB:" vocab ":")))
    (index-add! term refs pfx)))

(define (vocab-index-delete! vocab-name term)
  (index-delete! vocab-name term "%VOCAB:"))

(define (term-index-delete! vocab term refs)
  (let ((pfx (string-append "%VOCAB:" vocab ":")))
    (index-delete! term refs pfx)))

(define (vocab-index-exists? vocab val)
  (index-exists? vocab val "%VOCAB:"))

(define (term-index-exists? vocab term ref)
  (let ((pfx (string-append "%VOCAB:" vocab ":")))
    (index-exists? vocab term pfx)))

)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------


