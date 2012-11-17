;;; sukkiri-filestore.scm -- A file storage extension for Sukkiri. Using
;;;   this extension, you can interact with content stored in files as
;;;   if they were part of the database.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.

(module sukkiri-filestore
        *
        (import scheme)
        (import chicken)
        (import posix)
        (import files)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL DEFINITIONS  ------------------------------------------------

;; This is currently a toy implementation
(define *imt-prefixes*
  (make-parameter
    '((text . #x1000)
      (image . #x2000)
      (audio . #x3000)
      (video . #x4000)
      (application . #x5000))))

(define *file-storage-path* (make-parameter #f))

(define *version-control* (make-parameter 'git))

(define *use-version-control* (make-parameter #t))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

)

;;; ========================================================================
;;; ------------------------------------------------------------------------


