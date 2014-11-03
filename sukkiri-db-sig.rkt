;;; sukkiri-db-sig.rkt -- Signature for Sukkiri DB interface
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.


#lang racket

(define-signature sukkiri-db^
  ((contracted
    ;; Type predicates
    [sukkiri-db? (-> any/c boolean?)]
    [statement? (-> any/c boolean?)]
    [type-spec? (-> any/c boolean?)]
    [struct-member-spec? (-> any/c boolean?)]
    [struct-type-spec? (-> any/c boolean?)]
    [numeric-type-spec? (-> any/c boolean?)]
    [time-period? (-> any/c boolean?)]
    [db-value? (-> any/c boolean?)]

    ;; Type conversions
    [db->integer (-> db-value? integer?)]
    [db->float (-> db-value? flonum?)]
    [db->string (-> db-value? string?)]
    [db->boolean (-> db-value? boolean?)]
    [db->date (-> db-value? date?)]
    [db->time (-> db-value? time?)]
    [db->period (-> db-value? time-period?)]
    [integer->db (-> integer? db-value?)]
    [float->db (-> flonum? db-value?)]
    [string->db (-> string? db-value?)]
    [boolean->db (-> boolean? db-value?)]
    [date->db (-> date? db-value?)]
    [time->db (-> time? db-value?)]
    [period->db (-> time-period? db-value?)]
    [object->ext-type (-> statement? statement?)] 
  
    ;; General database manipulation
    ; Needs to be something like (-> connection-spec db)
    [create-db (-> any/c sukkiri-db?)]
    [connect (-> sukkiri-db? any)] 
    [disconnect (-> sukkiri-db? any)]
    [begin-transaction (-> sukkiri-db? any)]
    [commit (-> sukkiri-db? any)]
    [rollback (-> sukkiri-db? any)]
    [do-query (-> sukkiri-db? procedure? any)]
    ; User-defined type management
    [add-general-type (-> sukkiri-db? string? string? any)]
    [delete-general-type (->* (sukkiri-db? string?) (#:union boolean?) any)]
    [add-string-type (->* (sukkiri-db? string? string?) (#:description string?) any)]
    [add-number-type (->* (sukkiri-db?
                           string?)
                          (#:minval number?
                           #:maxval number?
                           #:step number?
                           #:digits integer?
                           #:description string?)
                          any)]
    [add-vocab-type (-> sukkiri-db? string? (listof string?))]
    [add-struct-type (->* (sukkiri-db?)
                          (#:extensible boolean?
                           #:members (listof struct-member-spec?)
                           #:description string?)
                          any)]
    [add-union-type (-> sukkiri-db? string? (listof string?) any)]
    [update-string-type (-> sukkiri-db? string? string? any)]
    [update-number-type (->* (sukkiri-db?
                              string?)
                             (#:minval number?
                              #:maxval number?
                              #:step number?
                              #:digits integer?)
                             any)]
    [update-vocab-type (->* (sukkiri-db? string?)
                            (#:terms+ (listof string?) #:terms- (listof string?))
                            any)]
    [update-struct-type (->* (sukkiri-db?
                              string?)
                             (#:extensible boolean?
                              #:members+ (listof struct-member-spec?)
                              #:members- (listof string?)
                              #:members* (listof struct-member-spec?))
                             any)]
    [update-union-type (->* (sukkiri-db?
                             string?)
                            (#:members+ (listof string?)
                             #:members- (listof string?))
                            any)]
    [delete-string-type (-> sukkiri-db? string? any)]
    [delete-number-type (-> sukkiri-db? string? any)]
    [delete-vocab-type (-> sukkiri-db? string? any)]
    [delete-struct-type (-> sukkiri-db? string? any)]
    [delete-union-type (-> sukkiri-db? string? any)]
    [get-string-type (-> sukkiri-db? string? string?)]
    [get-number-type (-> sukkiri-db? string? numeric-type-spec?)]
    [get-vocab-type (-> sukkiri-db? string? (listof string?))]
    [get-struct-type (-> sukkiri-db? string? struct-type-spec?)]
    [get-union-type (-> sukkiri-db? string? (listof string?))]
    [get-string-types (-> sukkiri-db? (listof string?))]
    [get-number-types (-> sukkiri-db? (listof string?))]
    [get-vocab-types (-> sukkiri-db? (listof string?))]
    [get-struct-types (-> sukkiri-db? (listof string?))]
    [get-union-types (-> sukkiri-db? (listof string?))]
    [get-type-class (-> sukkiri-db? string? string?)]
    [get-type (-> sukkiri-db? string? type-spec?)]

    ;; Statement manipulation
    [add-statement (-> sukkiri-db? string? string? string? string? any)]
    [add-statements (-> sukkiri-db? (listof statement?) any)]
    ; DB/FILE -> {SUBJ} -> {SUBJ_TYPE} -> {PRED} -> {OBJ} -> {TYPE} -> ()
    ; Wait: isn't that SUBJ_TYPE obsolete?
    [delete-statements (->* (sukkiri-db?)
                            (#:subj string?
                             #:pred string?
                             #:obj string?
                             #:type string?)
                            any)]
    [update-statement-object (-> sukkiri-db? string? string? string? any)]
    [statement-exists? (->* (sukkiri-db?)
                            (#:subj string?
                             #:pred string?
                             #:obj string?
                             #:type string?)
                            boolean?)]
    [get-statements (->* (sukkiri-db?)
                         (#:subj string?
                          #:pred string?
                          #:obj string?
                          #:type string?)
                         (listof statement?))])))

(provide sukkiri-db^)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  UTILITY FUNCTIONS  -----------------------------------------------
; 
; (define iso-format "~Y-~m-~dT~H:~M:~S")
; 
; (define << values)
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
; ;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
; 
;
; ;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
; ;;; ----  STATEMENT MANIPULATION  ------------------------------------------
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
