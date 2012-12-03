;;; sukkiri.scm -- A metadata storage system based on Redis.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.

(module sukkiri
        *
        (import scheme chicken)
        (require-library
          sukkiri-lib sukkiri-db sukkiri-tags sukkiri-vocabs)
        (reexport
          (except sukkiri-lib *sukkiri-debug*) 
          (except sukkiri-db *sukkiri-db-debug*) 
          (except sukkiri-tags *sukkiri-tags-debug*) 
          (except sukkiri-vocabs *sukkiri-vocabs-debug*))

) ; END MODULE
