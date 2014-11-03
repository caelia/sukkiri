;;; sukkiri-db-sig.rkt -- Signature for Sukkiri DB interface
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.


#lang racket

(define-signature sukkiri-db^
  ((contracted
    [sukkiri-db? (-> any/c boolean?)]
    ; Needs to be something like (-> connection-spec db)
    [create (-> any/c sukkiri-db?)]
    [connect (-> sukkiri-db? any)] 
    [disconnect (-> sukkiri-db? any)])))

(provide sukkiri-db^)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  UTILITY FUNCTIONS  -----------------------------------------------
; 
; (define iso-format "~Y-~m-~dT~H:~M:~S")
; 
; (define << values)
; 
; (define db->integer string->number)
; 
; (define db->float string->number)
; 
; (define db->string identity)
; 
; (define (db->boolean dbval)
;   (cond
;     ((string=? dbval "0") #f)
;     ((string=? dbval "1") #t)
;     (else (eprintf "'~A' is not a boolean value" dbval))))
; 
; (define (db->date dbval)
;   (string->date dbval iso-format))
; 
; (define (db->time dbval)
;   (date->time (string->date dbval iso-format)))
; 
; (define db->period string->number)
; 
; (define integer->db number->string)
; 
; (define float->db number->string)
; 
; (define string->db identity)
; 
; (define (boolean->db b)
;   (if b 1 0))
; 
; (define (date->db d)
;   (date->string d iso-format))
; 
; (define (time->db t)
;   (date->string (time->date t) iso-format))
; 
; ;; Currently a period is just a seconds value
; (define period->db identity)
; 
; (define validate-integer integer?)
; 
; (define validate-float flonum?)
; 
; (define validate-boolean boolean?)
; 
; (define validate-string string?)
; 
; (define validate-date date?)
; 
; (define validate-time time?)
; 
; (define validate-period number?)
; 
; (define (primitive? typespec)
;   (memv
;     (string->symbol typespec)
;     '(integer float boolean string date time period nref rref xref sref)))
; 
; (define (not-implemented . args)
;   (error "Not implemented."))
; 
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
; 
; 
; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  DATABASE SETUP  --------------------------------------------------
; 
; ;; FILENAME -> ()
; (define create-db (make-parameter not-implemented))
; 
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
; 
; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  USER-DEFINED TYPE MANAGEMENT  ------------------------------------
; 
; ;; DATABASE -> ()
; (define begin-transaction (make-parameter not-implemented))
; 
; ;; DB/FILE -> PROC -> ()
; (define do-query (make-parameter not-implemented))
; 
; ;; DATABASE -> TYPENAME -> TYPECLASS -> ()
; (define add-general-type (make-parameter not-implemented))
; 
; ;; DATABASE -> TYPENAME -> [UNION?] -> ()
; (define delete-general-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> PATTERN -> [DESCRIPTION] -> ()
; (define add-string-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> {MINVAL} -> {MAXVAL} -> {STEP} -> {DIGITS} -> {DESCRIPTION} -> ()
; (define add-number-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> TERMS -> ()
; (define add-vocab-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> {EXTENSIBLE} -> {MEMBERS} -> {DESCRIPTION} -> ()
; (define add-struct-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> MEMBERS -> ()
; (define add-union-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> PATTERN -> ()
; (define update-string-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> {MINVAL} -> {MAXVAL} -> {STEP} -> {DIGITS} -> ()
; (define update-number-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> {TERMS+} -> {TERMS-} -> ()
; (define update-vocab-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> {EXTENSIBLE} -> {MEMBERS+} -> {MEMBERS-} -> {MEMBERS*} -> ()
; (define update-struct-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME ->  {MEMBERS+} -> {MEMBERS-} -> ()
; (define update-union-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> ()
; (define delete-string-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> ()
; (define delete-number-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> ()
; (define delete-vocab-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> ()
; (define delete-struct-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> ()
; (define delete-union-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> PATTERN
; (define get-string-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> TYPEDEF
; (define get-number-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> MEMBERS
; (define get-vocab-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> TYPEDEF
; (define get-struct-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> MEMBERS
; (define get-union-type (make-parameter not-implemented))
; 
; ;; DB/FILE -> LIST
; (define get-string-types (make-parameter not-implemented))
; 
; ;; DB/FILE -> LIST
; (define get-number-types (make-parameter not-implemented))
; 
; ;; DB/FILE -> LIST
; (define get-vocab-types (make-parameter not-implemented))
; 
; ;; DB/FILE -> LIST
; (define get-struct-types (make-parameter not-implemented))
; 
; ;; DB/FILE -> LIST
; (define get-union-types (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> CLASSNAME
; (define get-type-class (make-parameter not-implemented))
; 
; ;; DB/FILE -> TYPENAME -> TYPEDEF
; (define get-type (make-parameter not-implemented))
; 
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
; 
; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  STATEMENT MANIPULATION  ------------------------------------------
; 
; ;; DB/FILE -> SUBJ -> PRED -> OBJ -> TYPE -> ()
; (define add-statement (make-parameter not-implemented))
; 
; ;; DB/FILE -> STATEMENTS -> ()
; (define add-statements (make-parameter not-implemented))
; 
; ;; DB/FILE -> {SUBJ} -> {SUBJ_TYPE} -> {PRED} -> {OBJ} -> {TYPE} -> ()
; ;; Wait: isn't that SUBJ_TYPE obsolete?
; (define delete-statements (make-parameter not-implemented))
; 
; ;; DB/FILE -> SUBJ -> PRED -> OBJ -> ()
; (define update-statement-object (make-parameter not-implemented))
; 
; ;; DB/FILE -> {SUBJ} -> {PRED} -> {OBJ} -> {TYPE} -> BOOL
; (define statement-exists? (make-parameter not-implemented))
; 
; ;; STATEMENT -> STATEMENT
; ;; Not sure this belongs in this section - isn't it higher-level?
; (define (object->ext-type statement)
;   (let* ((subject (alist-ref 's statement))
;          (prop (alist-ref 'p statement))
;          (type (alist-ref 't statement))
;          (raw-object (alist-ref 'o statement))
;          (object
;            (case (string->symbol type)
;              ((integer) (db->integer raw-object))
;              ((float) (db->float raw-object))
;              ((boolean) (db->boolean raw-object))
;              ((date) (db->date raw-object))
;              ((time) (db->time raw-object))
;              ((period) (db->period raw-object))
;              (else raw-object))))
;     `((s . ,subject) (p . ,prop) (o . ,object))))
; 
; ;; DB/FILE -> {SUBJ} -> {PRED} -> {OBJ} -> {TYPE} -> STATEMENTS
; (define get-statements (make-parameter not-implemented))
;       
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
; 
; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  HIGH-LEVEL INTERFACE  --------------------------------------------
; 
; (define (prepare-object db/file type obj)
;   (let ((class (get-type-class db/file type)))
;     (cond
;       ((equal? type "boolean") (values type (boolean->db obj)))
;       ((equal? type "date") (values type (date->db obj)))
;       ((equal? type "time") (values type (time->db obj)))
;       ((equal? type "period") (values type (period->db obj)))
;       ((equal? class "struct") (values "nref" (add-struct db/file obj)))
;       (else (values type obj)))))
;  
; (define (flatten-list-objects db/file str)
;   (let loop ((stmts-in str) (stmts-out '()))
;     (if (null? stmts-in)
;       stmts-out
;       (let ((p (caar stmts-in))
;             (o (cdar stmts-in)))
;         (if (list? o)
;           (loop
;             (cdr stmts-in)
;             (append stmts-out
;               (map (lambda (o*) `(,p . ,o*)) o)))
;           (loop
;             (cdr stmts-in)
;             (cons `(,p . ,o) stmts-out)))))))
; 
; (define (add-struct db/file str)
;   (let ((id (alist-ref '%ID str))
;         (type (alist-ref '%TYPE str))
;         (members
;           (remove
;             (lambda (elt) (eqv? (car elt) '%ID))
;             str)))
;     (add-statements db/file (map (lambda (m) (cons id m)) members))))
; 
; (define (get-struct db/file id)
;   (let ((statements (get-statements db/file s: id)))
;     (cons
;       `(%ID . ,id) 
;       (map
;         (lambda (elt)
;           `(,(alist-ref 'p elt) . ,(alist-ref 'o elt)))
;         statements))))
; 
; (define (init-store filespec #!optional (replace #f))
;   (%db-file% filespec)
;   (when replace
;     (delete-file* filespec))
;   (unless (file-exists? filespec)
;     (create-db filespec)))
; 
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
; ) ; END MODULE
; 
; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ------------------------------------------------------------------------
; 
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
; ;;; ========================================================================
; ;;; ------------------------------------------------------------------------
; 
