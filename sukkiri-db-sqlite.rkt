#lang racket

(require (rename-in db [disconnect db-disconnect]))
(require "sukkiri-db-sig.rkt")

(define-unit sukkiri-db-sqlite@
  (import)
  (export sukkiri-db^)

  (struct sukkiri-sqlite-db (file [connection #:mutable]))

  (define sukkiri-db? sukkiri-sqlite-db?)

  (define (create args)
    (let* ((file-pair (assoc 'file args))
           (file (and file-pair (cdr file-pair))))
      (if file
        (sukkiri-sqlite-db file #f)
        (raise "You must specify a filename."))))

  (define (connect dbo)
    (unless (sukkiri-sqlite-db-connection dbo)
      (let* ((file
               (sukkiri-sqlite-db-file dbo))
             (mode
                (if (file-exists? file)
                  'read/write
                  'create))
             (conn
               (sqlite3-connect #:database file #:mode mode)))
        (set-sukkiri-sqlite-db-connection! dbo conn))))

  (define (disconnect dbo)
    (let ((conn (sukkiri-sqlite-db-connection dbo)))
      (when conn
        (db-disconnect conn)
        (set-sukkiri-sqlite-db-connection! dbo #f)))))

(provide sukkiri-db-sqlite@)
