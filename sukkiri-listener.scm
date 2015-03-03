;;; sukkiri-listener.scm -- RESTful request handler for Sukkiri
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the BSD
;;;   license. See the accompanying LICENSE file for details.

(module sukkiri-listener
        *
        (import scheme chicken)
        (use (prefix sukkiri-base b:))
        (use (prefix sukkiri-store s:))
        (use (prefix sukkiri-data d:))
        (use (prefix sukkiri-query q:))
        (use (prefix medea m:))
        (use yasos)
        (use zmq)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define-predicate sukkiri-listener?)

(define-operation (HEAD listener uri))
(define-operation (GET listener uri))
(define-operation (PUT listener uri data))
(define-operation (DELETE listener uri))
(define-operation (POST listener uri data))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; end module
