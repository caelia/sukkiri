;;; sukkiri-ajax.scm -- JSON I/O routines for Sukkiri
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

;;; Example JSON Inputs
;;;
;;; { "%TYPE": "project",
;;;   "%ID": "breed-mutant-seahorses",
;;;   "%LABEL": "title",
;;;   "title": "Breed Mutant Seahorses",
;;;   "deadline": "2014-03-28T17:00:00",
;;;   "contact": "%NREF%jane-morgan",
;;;   "step": ["%NREF%build-tank", "%NREF%catch-seahorses", "%NREF%assign-to-tanks"] }
;;;
;;; { "%TYPE": "action",
;;;   "%ID": "build-tank",
;;;   "%LABEL": "content",
;;;   "content": "Build tank for seahorses",
;;;   "context": ["lab"] }
;;;
;;; { "%TYPE": "person",
;;;   "%ID": "jane-morgan",
;;;   "%LABEL": "given-name & surname",
;;;   "given-name": "Jane",
;;;   "surname": "Morgan",
;;;   "email": ["%NREF%jane-morgan-work-email"] }
;;;
;;; { "%TYPE": "email-address",
;;;   "%ID": "jane-morgan-work-email",
;;;   "label": "Work",
;;;   "address": "dr.jane.v.morgan@sea-creature-research.com" }

(module sukkiri-ajax
        *
        (import scheme chicken)
        (use (prefix sukkiri-base b:))
        (use (prefix sukkiri-store s:))
        (use (prefix sukkiri-data d:))
        (use (prefix medea m:))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  INPUT FROM JSON  -------------------------------------------------

(define (json->db port)
  (let ((raw-struct (m:read-json port)))
    (d:store-struct raw-struct)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  OUTPUT TO JSON  --------------------------------------------------

(define (db->json id port)
  (let ((raw-struct (d:retrieve-struct id)))
    (m:write-json raw-struct port)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

