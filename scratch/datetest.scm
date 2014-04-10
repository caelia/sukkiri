(use srfi-1)
(use srfi-19)
(use srfi-19-io)
(use s11n)
(use posix)

(define iso-format "~Y-~m-~dT~H:~M:~S")
(define friendly-format "~H:~M:~S, ~A, ~B ~e, ~Y")

(define (ndates-srfi n)
  (list-tabulate
    n
    (lambda (_)
      (let ((nanos (random 1000000000))
            (secs (random 60))
            (mins (random 60))
            (hrs (random 24))
            (days (+ (random 28) 1))
            (mos (+ (random 12) 1))
            (yrs (+ (random 200) 1900)))
        (make-date nanos secs mins hrs days mos yrs)))))

(define (strings->dates/iso ss)
  (map
    (lambda (s) (string->date s iso-format))
    ss))

(define (strings->dates/friendly ss)
  (map
    (lambda (s) (string->date s friendly-format))
    ss))

(define (dates->strings/iso dd)
  (map
    (lambda (d) (date->string d iso-format))
    dd))

(define (dates->strings/friendly dd)
  (map
    (lambda (d) (date->string d friendly-format))
    dd))

(define (sers->dates ss)
  (map
    (lambda (s)
      (with-input-from-string s
        (lambda () (deserialize))))
    ss))

(define (dates->sers dd)
  (map
    (lambda (d)
      (with-output-to-string
        (lambda () (serialize d))))
    dd))

(define (rt-iso dd)
  (strings->dates/iso (dates->strings/iso dd)))

(define (rt-friendly dd)
  (strings->dates/friendly (dates->strings/friendly dd)))

(define (rt-ser dd)
  (sers->dates (dates->sers dd)))

(define-syntax time-run
  (syntax-rules ()
    ((_ form1 form2 ...)
     (let ((start-time (current-seconds)))
       form1
       form2
       ...
       (let ((end-time (current-seconds)))
         (printf "Elapsed-time: ~A\n" (- end-time start-time)))))))
