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
  (redis-hsetnx s p o))

(define (update-triple s p o)
  (redis-hset s p o))

(define (delete-triple s p)
  (redis-hdel s p))

(define (select s p o #!optional result-spec)
  (let ((result '())
        (hash-keys
          (if (string=? s "*")
            (filter
              (lambda (x)
                (and (not (string=? x "@CONFIG"))
                     (string=? (car (redis-type x)) "hash")))
              (redis-keys "*"))
            (list s))))


)
