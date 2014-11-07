;;; sukkiri-db-sqlite.scm -- SQLite3 implementation for Sukkiri database layer
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the BSD license.
;;;   See the accompanying LICENSE file for details.
#lang racket

(require (rename-in db
                    [disconnect db-disconnect]
                    [statement? db-statement?]))
(require "sukkiri-db-sig.rkt")

(define-unit sukkiri-db-sqlite@
  (import)
  (export sukkiri-db^)

  (include "sukkiri-common-sql.rkt")

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
        (set-sukkiri-sqlite-db-connection! dbo conn)
        conn)))

  (define (disconnect dbo)
    (let ((conn (sukkiri-sqlite-db-connection dbo)))
      (when conn
        (db-disconnect conn)
        (set-sukkiri-sqlite-db-connection! dbo #f)))))


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE SETUP  --------------------------------------------------

(define (@setup-db dbo)
  (let* ((conn (connect dbo))
         (qx (λ (q) (query-exec conn q))))
    (call-with-transaction
      (λ ()
        (for-each
          qx
          `(,create-primitive-table-query
            ,create-string-type-table-query
            ,create-number-type-table-query
            ,create-vocab-table-query
            ,create-cardinality-table-query
            ,create-struct-type-table-query
            ,create-type-class-table-query
            ,create-types-table-query
            ,create-union-type-table-query
            ,create-struct-members-table-query
            ,create-statement-table-query))
        (for-each
          (λ (qlist) (for-each qx qlist))
          `(,populate-primitive-table-queries
            ,populate-cardinality-table-queries
            ,populate-type-class-table-queries
            ,populate-types-table-queries
            ,populate-union-type-table-queries)))))
  (disconnect dbo))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO




(provide sukkiri-db-sqlite@)
