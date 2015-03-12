
(define-record-type char-seq-cursor
  (make-char-seq-cursor buffer pos next)
  char-seq-cursor?
  (buffer csc-buffer)
  (pos csc-pos)
  (next csc-next))

(define end-of-char-seq-cursor
  (delay (make-char-seq-cursor #f 0 #f)))

(define (read-chunk chunk-size in)
  (and (not (eof-object? (peek-char in)))
       (if chunk-size
           (let ((chunk (read-buffered in)))
             (if (string-null? chunk)
                 (let ((chunk (read-string chunk-size in)))
                   (and (not (string-null? chunk)) chunk))
                 chunk))
           (read-string 1 in))))

(define (char-seq-cursor x chunk-size)
  (cond ((string? x)
         (make-char-seq-cursor x 0 end-of-char-seq-cursor))
        ((input-port? x)
         (let loop ()
           (make-char-seq-cursor (read-chunk chunk-size x) 0 (delay (loop)))))
        (else (error "Unable to convert object to char-seq-cursor" x))))

(define (char-seq-cursor-next csc)
  (force (csc-next csc)))

(define (char-seq-cursor-next* csc)
  (let ((next (char-seq-cursor-next csc)))
    (and (csc-buffer next) next)))

(define (char-seq-cursor-end? csc)
  (let ((buf (csc-buffer csc)))
    (or (not buf)
        (and (= (csc-pos csc) (string-length buf))
             (not (csc-buffer (char-seq-cursor-next csc)))))))

(define (char-seq-cursor-chunk csc)
  (and-let* ((buf (csc-buffer csc))
             (pos (csc-pos csc)))
    (substring/shared buf pos)))

(define (char-seq-cursor-skip csc n)
  (let loop ((csc csc)
             (n n))
    (and-let* ((buf (csc-buffer csc))
               (pos (csc-pos csc))
               (len (- (string-length buf) pos)))
      (if (> len n)
          (make-char-seq-cursor buf (+ pos n) (csc-next csc))
          (loop (char-seq-cursor-next csc)
                (- n len))))))

(define-record-printer (char-seq-cursor csc out)
  (display "#<char-seq-cursor" out)
  (let ((buf (csc-buffer csc)))
    (if buf
        (let* ((pos (csc-pos csc))
               (len (string-length buf))
               (max-len 30)
               (truncate? (> len max-len)))
          (display " " out)
          (write (substring/shared buf pos (if truncate? max-len len)) out)
          (when (or truncate?
                    (csc-buffer (char-seq-cursor-next csc)))
            (display " ..." out)))
        (display "-end" out)))
  (display ">" out))
