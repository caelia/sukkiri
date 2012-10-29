;;; sukkiri.scm -- A metadata store based on Redis

(module sukkiri *
        (import scheme)
        (import chicken)
        (import redis-client)
        (import redis-extras)
        (import srfi-1)

(define S 1)
(define P 2)
(define O 4)

(define (add-triple s p o)
  (redis-transaction
    (redis-hsetnx s p o)
    (redis-sadd "@SUBJECTS" s)))

(define (update-triple s p o)
  (redis-transaction
    (redis-hset s p o)
    (redis-sadd "@SUBJECTS" s))

(define (delete-triple s p)
  (redis-hdel s p)
  (when (null? (redis-hkeys s))
    (redis-srem "@SUBJECTS" s)))

(define (select:s:p:* s p)
  (car (redis-hget s p)))

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
          (loop (cdr o) (cons res results)))))))

(define (get-o-sel o)
  (cond
    ((list? o) select:s:p:o+)
    ((string=? o "*") select:s:p:*)
    (else select:s:p:o)))

(define (select:s:p+ s p+ o)
  (let ((sel (get-o-sel o)))
    (map
      (lambda (p)
        (map
          (lambda (o*) (list p o*))
          (sel s p o)))
      p+)))

(define (select:s:* s _ o)
  (select:s:p+ s (redis-hkeys s) o))

(define (select:s:p s p o)
  (let ((sel (get-o-sel o)))
    (list
      (map 
        (lambda (o*) (list p o*))
        (sel s p o)))))

(define (get-p-sel p)
  (cond
    ((list? p) select:s:p+)
    ((string=? p "*") select:s:*)
    (else select:s:p)))

(define (select:s+ s+ p o)
  (let ((sel (get-p-sel p)))
    (map
      (lambda (s)
        (map
          (lambda (po) (cons s po))
          (sel s p o)))
      s+)))

(define (select:s s p o)
  (let ((sel (get-p-sel p)))
    (list
      (map
        (lambda (po) (cons s po))
        (sel s p o)))))

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

(define (select s p o #!optional result-spec)
  (let* ((sel
           (cond
             ((list? s) select:s+)
             ((string=? s "*") select:*)
             (else select:s)))
         (res (sel s p o)))
    ;;; XXX Placeholder ;;;
    #f))


)
