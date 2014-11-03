#lang racket

(require (rename-in db [disconnect db-disconnect]))
(require "sukkiri-db-sig.rkt")

(define-unit sukkiri-db-postgres@
  (import)
  (export sukkiri-db^)

  (struct sukkiri-postgres-db (dbname username password [connection #:mutable]))

  (define sukkiri-db? sukkiri-postgres-db?)

  (define (create args)
    (let* ((dbname-pair (assoc 'dbname args))
           (username-pair (assoc 'username args))
           (password-pair (assoc 'password args))
           (dbname (and dbname-pair (cdr dbname-pair)))
           (username (and username-pair (cdr username-pair)))
           (password (and password-pair (cdr password-pair))))
      (if (and dbname username password)
        (sukkiri-postgres-db dbname username password #f)
        (raise "You must specify a database name, username, and password."))))

  (define (connect dbo)
    (unless (sukkiri-postgres-db-connection dbo)
      (let* ((dbname (sukkiri-postgres-db-dbname dbo))
             (username (sukkiri-postgres-db-username dbo))
             (password (sukkiri-postgres-db-password dbo))
             (conn (postgresql-connect #:database dbname #:user username #:password password)))
        (set-sukkiri-postgres-db-connection! dbo conn))))

  (define (disconnect dbo)
    (let ((conn (sukkiri-postgres-db-connection dbo)))
      (when conn
        (db-disconnect conn)
        (set-sukkiri-postgres-db-connection! dbo #f)))))

(provide sukkiri-db-postgres@)
