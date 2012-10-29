;;; sukkiri.scm -- A metadata store based on Redis

(module sukkiri
        (add-triple
         update-triple
         delete-triple
         select
         init
         :>
         :>>
         :-
         :<)

        (import scheme)
        (import chicken)
        (import extras)
        (import redis-client)
        (import srfi-1)
        (import srfi-69)


;;; {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
;;; --  GLOBAL PARAMETERS  ---------------------------------------------

;; FIXME -- obviously this should not be hard-coded. Need to figure out how
;;   to query Redis for this info. (redis-config "get" "databases") doesn't
;;   appear to work.
(define *total-dbs* (make-parameter 16))

(define *default-host* (make-parameter "localhost"))

(define *default-port* (make-parameter 6379))

(define *connected* (make-parameter #f))

(define *current-app* (make-parameter #f))

(define *redis-extras-debug* (make-parameter #f))

;;; }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



;;; {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
;;; --  UTILITY FUNCTIONS  -----------------------------------------------------

(define (debug-msg . msgs)
  (when (*redis-extras-debug*)
    (apply print msgs)))

;;; }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



;;; {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
;;; --  AUTOMATIC DB ALLOCATION  ---------------------------------------

(define (first-available-index indices)
  (let ((indices* (map string->number indices))
        (last-idx (- (*total-dbs*) 1)))
    (let loop ((i 1))
      (cond
        ((> i last-idx) #f)
        ((memv i indices*) (loop (+ i 1)))
        (else i)))))

(define (get-db-index app-id)
  (debug-msg "get-db-index")
  (redis-select "0")
  (let ((exists (car (redis-exists app-id))))
    (and (= exists 1)
         (car (redis-hget app-id "db-index")))))

(define (allocate-db app-id)
  (debug-msg "allocate-db")
  (redis-select "0")
  (let* ((allocated-dbs (redis-smembers "dbs-in-use"))
         (available-index (first-available-index allocated-dbs)))
    (if available-index
      (let ((index (number->string available-index)))
        (redis-transaction
          (redis-hset app-id "db-index" index)
          (redis-sadd "dbs-in-use" index))
        index)
      (abort "No dbs available."))))

(define (deallocate-db app-id)
  (let ((index (get-db-index app-id)))
    (redis-transaction
      (redis-select index)
      (redis-flushdb)
      (redis-select "0")
      (redis-hdel app-id "db-index")
      (redis-srem "dbs-in-use" index))))

(define (with-db-select thunk)
  (let ((app (*current-app*)))
    (when (not app)
      (abort "Current app is not set."))
    (let ((idx (get-db-index app)))
      (redis-select idx)
      (thunk))))

(define (redex-init #!optional app-id #!key (host (*default-host*)) (port (*default-port*)))
  (debug-msg "redex-init")
  (when (not (*connected*))
    (debug-msg "redex-init: not connected")
    (redis-connect host port)
    (debug-msg "redex-init: redis-connect done")
    (*connected* #t))
  (debug-msg "redex-init: connected")
  (redis-select "0")
  (debug-msg "redex-init: select db 0")
  (let ((in-use-exists (redis-exists "dbs-in-use")))
    (debug-msg "redex-init: test for 'dbs-in-use'")
    (debug-msg "is 'dbs-in-use' a number?" (number? (car in-use-exists)))
    (when (= (car in-use-exists) 0)
      (debug-msg "redex-init: dbs-in-use does not exist; need to add it")
      (redis-sadd "dbs-in-use" "0"))
    (debug-msg "'in-use-exists' now exists")
    (if app-id
      (begin
        (debug-msg "there is an app id")
        (*current-app* app-id)
        (let ((index (or (get-db-index app-id)
                         (allocate-db app-id))))
          (debug-msg "got index; now select")
          (redis-select index)
          (debug-msg "selected")
          index))
      #t)))

;;; }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



;;; {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
;;; --  GENERIC STORAGE PROTOCOL  --------------------------------------

(define (boolean->string b)
  (if b "T" "F"))

(define (string->boolean s)
  (cond
    ((string=? s "T") #t)
    ((string=? s "F") #f)
    (else (error (sprintf "String '~A' does not represent a boolean.")))))

(define (obj->string obj)
  (with-output-to-string
    (lambda () (serialize obj))))

(define (string->obj s)
  (with-input-from-string s
    (lambda () (deserialize))))

(define (any->dbstring obj)
  (let* ((prefix+conv
           (cond
             ((null? obj) (cons #\- (lambda (_) "")))
             ((boolean? obj) (cons #\b boolean->string))
             ((number? obj) (cons #\n number->string))
             ((char? obj) (cons #\c ->string))
             ((string? obj) (cons #\s identity))
             ((list? obj)
              (cons #\L store-indirect-list))
             ((hash-table? obj)
              (cons #\H store-indirect-hash))
             (else (cons #\O obj->string))))
         (prefix (car prefix+conv))
         (converter (cdr prefix+conv)))
    (list->string (cons prefix (string->list (converter obj))))))

(define (dbstring->any s)
  (let ((first (string-ref s 0))
        (rest (substring s 1)))
    (case first
      ((#\-) '())
      ((#\b) (string->boolean rest))
      ((#\n) (string->number rest))
      ((#\c) (string-ref rest 0))
      ((#\s) rest)
      ((#\L) (retrieve-indirect-list rest))
      ((#\H) (retrieve-indirect-hash rest))
      ((#\O) (string->obj rest))
      (else (error "Unknown data type.")))))

;;; }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



;;; {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
;;; --  LOW-LEVEL QUERY API  -------------------------------------------

(define (add-triple s p o)
  (redis-transaction
    (redis-hsetnx s p o)
    (redis-sadd "@SUBJECTS" s)))

(define (update-triple s p o)
  (redis-transaction
    (redis-hset s p o)
    (redis-sadd "@SUBJECTS" s)))

(define (delete-triple s p)
  (redis-hdel s p)
  (when (null? (redis-hkeys s))
    (redis-srem "@SUBJECTS" s)))

(define (select:s:p:* s p _)
  (redis-hget s p))

(define (select:s:p:o s p o)
  (let ((db-result (car (redis-hget s p))))
    (cond
      ((null? db-result) '())
      ((string=? db-result o) (list db-result))
      (else '()))))

(define (select:s:p:o+ s p o+)
  (let loop ((o o+)
             (results '()))
    (if (null? o)
      (reverse results)
      (let ((res (select:s:p:o s p (car o))))
        (if (null? res)
          (loop (cdr o) results)
          (loop (cdr o) (cons (car res) results)))))))

(define (get-o-sel o)
  (cond
    ((list? o) select:s:p:o+)
    ((string=? o "*") select:s:p:*)
    (else select:s:p:o)))

(define (select:s:p+ s p+ o)
  (let ((sel (get-o-sel o)))
    (let loop ((p p+)
               (results '()))
      (if (null? p)
        results
        (let* ((p* (car p))
               (res (sel s p* o)))
          (if (null? res)
            (loop (cdr p) results)
            (let ((res*
                    (map
                      (lambda (o*) (list p* o*))
                      res)))
              (loop (cdr p) (append results res*)))))))))


(define (select:s:* s _ o)
  (select:s:p+ s (redis-hkeys s) o))

(define (select:s:p s p o)
  (let ((sel (get-o-sel o)))
    (map
      (lambda (o*) (list p o*))
      (sel s p o))))

(define (get-p-sel p)
  (cond
    ((list? p) select:s:p+)
    ((string=? p "*") select:s:*)
    (else select:s:p)))

(define (select:s+ s+ p o)
  (let ((sel (get-p-sel p)))
    (let loop ((s s+)
               (results '()))
      (if (null? s)
        results
        (let* ((s* (car s))
               (res (sel s* p o)))
          (if (null? res)
            (loop (cdr s) results)
            (let ((res*
                    (map
                      (lambda (po*) (cons s* po*))
                      res)))
              (loop (cdr s) (append results res*)))))))))
                        
(define (select:s s p o)
  (let ((sel (get-p-sel p)))
    (map
      (lambda (po) (cons s po))
      (sel s p o))))

;; This could be deadly for performance
; (define (all-hash-keys)
;   (filter
;     (lambda (x)
;       (and (not (string=? x "@CONFIG"))
;            (not (string=? x "@SUBJECTS"))
;            (string=? (car (redis-type x)) "hash")))
;     (redis-keys "*")))

(define (all-hash-keys)
  (redis-smembers "@SUBJECTS"))

(define (select:* _ p o)
  (select:s+ (all-hash-keys) p o))

(define (filter-result res filter-spec unique)
  (let ((f
          (case filter-spec
            ((all) (lambda (x) x))
            ((sp) (lambda (x) (list (car x) (cadr x))))
            ((so) (lambda (x) (list (car x) (caddr x))))
            ((po) (lambda (x) (list (cadr x) (caddr x))))
            ((s) (lambda (x) (car x)))
            ((p) (lambda (x) (cadr x)))
            ((o) (lambda (x) (caddr x)))
            (else #f))))
    (cond 
      ((and f unique) (delete-duplicates (map f res)))
      (f (map f res))
      (else '()))))

(define (select s p o #!key (filter 'all) (unique #t))
  (let* ((sel
           (cond
             ((list? s) select:s+)
             ((string=? s "*") select:*)
             (else select:s)))
         (res (sel s p o)))
    (filter-result res filter unique)))

;; Useful (?) aliases
(define :> add-triple)
(define :>> update-triple)
(define :- delete-triple)
(define :< select)

;;; }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


)


;;; ====================================================================
