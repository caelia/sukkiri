(use srfi-13)

(define files 
  '("application.txt" "audio.txt" "image.txt" "message.txt"
    "model.txt" "multipart.txt" "text.txt" "video.txt"))

(define (zp nstr #!optional (len 4))
  (let loop ((nstr* nstr))
    (if (>= (string-length nstr*) len)
      nstr*
      (loop (string-append "0" nstr*)))))

(define (pair-string name n)
  (let* ((hexstr (number->string n 16))
         (hexstr* (zp hexstr)))
    (sprintf "(\"~A\" . #x~A)\n" name hexstr*)))

(define (read-line*)
  (let ((raw-line (read-line)))
    (if (eof-object? raw-line)
      raw-line
      (let ((line (string-trim-both raw-line)))
        (and (> (string-length line) 0)
             line)))))

(define (make-output-name fname)
  (call-with-values
    (lambda () (decompose-pathname fname))
    (lambda (dir file ext)
      (let ((newfn (string-append file "-out")))
        (make-pathname dir newfn ext)))))

(define (process-file fname)
  (let ((newname (make-output-name fname)))
    (with-output-to-file
      newname
      (lambda ()
        (with-input-from-file
          fname
          (lambda ()
            (let loop ((line (read-line*))
                       (n 1))
              (cond
                ((eof-object? line)
                 #f)
                ((not line)
                 (loop (read-line*) n))
                (else
                  (display (pair-string line n))
                  (loop (read-line*) (+ n 1)))))))))))

(define (run)
  (for-each process-file files))
