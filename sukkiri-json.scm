;;; sukkiri-json.scm -- JSON I/O routines for Sukkiri
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

;;; Example JSON Inputs
;;;
;;; { "%TYPE": "project",
;;;   "%ID": "breed-mutant-seahorses",
;;;   "title": "Breed Mutant Seahorses",
;;;   "deadline": "2014-03-28T17:00:00",
;;;   "contact": "%NREF%jane-morgan",
;;;   "step": ["%NREF%build-tank", "%NREF%catch-seahorses", "%NREF%assign-to-tanks"] }
;;;
;;; { "%TYPE": "action",
;;;   "%ID": "build-tank",
;;;   "content": "Build tank for seahorses",
;;;   "context": ["lab"] }
;;;
;;; { "%TYPE": "person",
;;;   "%ID": "jane-morgan",
;;;   "given-name": "Jane",
;;;   "surname": "Morgan",
;;;   "email": ["%NREF%jane-morgan-work-email"] }
;;;
;;; { "%TYPE": "email-address",
;;;   "%ID": "jane-morgan-work-email",
;;;   "label": "Work",
;;;   "address": "dr.jane.v.morgan@sea-creature-research.com" }

(module sukkiri-json
        *
        (import scheme chicken)
        (use sukkiri-base)
        (use medea)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  INPUT FROM JSON  -------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  OUTPUT TO JSON  --------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

