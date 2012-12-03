;;; sukkiri-tags.scm -- Adds tagging capability to Sukkiri
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD License. See the accompanying LICENSE file for details.

(module sukkiri-tags
        *

        (import scheme)
        (import chicken)
        (import extras)
        (import data-structures)
        (import ports)
        (import srfi-1)
        (import srfi-69)

        (use redis-client)
        (use numbers)
        (use sets)
        (use irregex)
        (use sukkiri-lib)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL PARAMETERS  ---------------------------------------------

(define *sukkiri-tags-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  UTILITY FUNCTIONS  ---------------------------------------------

(define (debug-msg . msgs)
  (when (*sukkiri-tags-debug*)
    (with-output-to-port
      (current-error-port)
      (lambda () (apply print msgs)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  DATABASE INDEXES  ----------------------------------------------

;; FIXME: Think we should URLencode the string to handle spaces
(define (tag-index-add! tag #!optional (refs '()))
  (index-add! tag refs prefix: "%TAG:"))

(define (tag-index-delete! name value)
  (index-delete! name value "%TAG:"))

(define (tag-index-exists? name value)
  (index-exists? name value "%TAG:"))

(define (get-tag-index name)
  (get-index name "%TAG:"))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  INITIALIZATION  ------------------------------------------------

(register-set-type 'simple-tag-set 'string
                   post-hook:
                   (lambda (res-id _ new-val)
                     (set-for-each
                       (lambda (m)
                         (tag-index-add! m (list res-id)))
                       new-val)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ====================================================================
;;; --------------------------------------------------------------------
