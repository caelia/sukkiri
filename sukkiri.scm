;;; sukkiri.scm -- A metadata store based on Redis

(module sukkiri *
;        (add-triple
;         update-triple
;         delete-triple
;         select
;         init
;         :>
;         :>>
;         :-
;         :<)

        (import scheme)
        (import chicken)
        (import extras)
        (import redis-client)
        (import redis-extras)
        (import srfi-1)

(define S 1)
(define P 2)
(define O 4)
(define ALL (bitwise-ior S P O))

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

;        (let loop2 ((res (sel s p o))
;                    (results* '()))
;          (if (null? res)
;            (loop1 (cdr p) (cons (reverse results*) results))
;            (let* ((p* (car p))
;                   (res (sel s p* (car o*))))
;              (if (null? res)
;                (loop2 (cdr o*) results*)
;                (loop2 (cdr o*) (cons (cons p* res) results*))))))))))

;    (map
;      (lambda (p)
;        (map
;          (lambda (o*) (list p o*))
;          (sel s p o)))
;      p+)))

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
                        
;    (map
;      (lambda (s)
;        (map
;          (lambda (po) (cons s po))
;          (sel s p o)))
;      s+)))

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

(define (init dbname)
  (redex-init dbname))

)
