;;; sukkiri-db.scm -- Redis interface layer for Sukkiri
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD License. See the accompanying LICENSE file for details.
;;;
;;; DB Administration & Sessions
;;; ============================
;;; Although you may use Sukkiri to store and query data with a simple
;;; connection to any Redis server (i.e., using REDIS-CONNECT from
;;; the redis-client extension, you may wish to segregate your data
;;; from other applications that may be using the same server. To
;;; facilitate this process, this library provides the INIT-SUKKIRI-DBS,
;;; START-SUKKIRI-SERVER, and OPEN-SUKKIRI-DB procedures. The egg also
;;; includes the 'sukkiri-admin' program, which provides a command-line
;;; interface to the first two of these procedures.

(module sukkiri-db
;        *
        (*total-dbs*
         *default-host*
         *default-port*
         *control-db-idx*
         *scratch-db-idx*
         *connected*
         *current-app*
         *sukkiri-db-debug*
         allocate-db
         deallocate-db
         with-db-select
         open-db-session)

        (import scheme)
        (import chicken)
        (import extras)
        (import data-structures)
        (import ports)
        (import srfi-1)
        (import srfi-69)

        (use redis-client)
        ;(use srfi-19)
        ;(use numbers)
        ;(use sets)
        ;(use irregex)
        ;(use s11n) ; FIXME -- using because date->secstring is not working


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL PARAMETERS  ---------------------------------------------

;; Set a reasonable default in case someone is using an older version of
;;   Redis, one that doesn't support CONFIG GET databases. For those who
;;   have an up-to-date version of Redis, this parameter will be set to
;;   the actual configured value at runtime.
(define *total-dbs* (make-parameter 16))

(define *default-host* (make-parameter "localhost"))

(define *default-port* (make-parameter 6379))

(define *control-db-idx* (make-parameter "1"))

(define *scratch-db-idx* (make-parameter "0"))

(define *connected* (make-parameter #f))

(define *current-app* (make-parameter #f))

(define *sukkiri-db-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  UTILITY FUNCTIONS  ---------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

(define (debug-msg . msgs)
  (when (*sukkiri-db-debug*)
    (with-output-to-port
      (current-error-port)
      (lambda () (apply print msgs)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  AUTOMATIC DB ALLOCATION  ---------------------------------------

(define (init-dbs #!optional (force #f))
  (let ((db-empty?
          (lambda () (null? (redis-keys "*")))))
    (redis-select (*control-db-idx*))
    (cond
      ((db-empty?) #t)
      (force (redis-flushdb))
      (else (error "Database contains data; unable to initialize.")))
    (redis-sadd "dbs-in-use" (*control-db-idx*))
    (redis-sadd "dbs-in-use" (*scratch-db-idx*))
    (redis-set "%CONTROL-DB" "1")
    (redis-hset "scratch" "db-index" (*scratch-db-idx*))))

(define (set-total-dbs)
  ;; This query works w/ Redis 2.6, not 2.4.
  (let ((result (redis-config "get" "databases")))
    (when (not (null? result))
      (*total-dbs* (string->number (cadr result))))))

(define (first-available-index indices)
  (let ((indices* (map string->number indices))
        (last-idx (- (*total-dbs*) 1)))
    (let loop ((i 2))
      (cond
        ((> i last-idx) #f)
        ((memv i indices*) (loop (+ i 1)))
        (else i)))))

(define (get-db-index app-id)
  (debug-msg "get-db-index")
  (redis-select (*control-db-idx*))
  (let ((exists (car (redis-exists app-id))))
    (and (= exists 1)
         (car (redis-hget app-id "db-index")))))

(define (allocate-db app-id)
  (debug-msg "allocate-db")
  (redis-select (*control-db-idx*))
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
      (redis-select (*control-db-idx*))
      (redis-hdel app-id "db-index")
      (redis-srem "dbs-in-use" index))))

(define (with-db-select thunk)
  (let ((app (*current-app*)))
    (when (not app)
      (abort "Current app is not set."))
    (let ((idx (get-db-index app)))
      (redis-select idx)
      (thunk))))

(define (open-db-session #!optional app-id
                   #!key (host (*default-host*)) (port (*default-port*))
                   (force-init #f))
  (debug-msg "open-db-session")
  (when (not (*connected*))
    (debug-msg "open-db-session: not connected")
    (redis-connect host port)
    (debug-msg "open-db-session: redis-connect done")
    (*connected* #t))
  (debug-msg "open-db-session: connected")
  (set-total-dbs)
  (redis-select (*control-db-idx*)) ; Select control DB
  (debug-msg "open-db-session: select control db")
  (let ((rs-exists (redis-exists "%CONTROL-DB")))
    (when (= (car rs-exists) 0)
      (debug-msg "open-db-session: '%CONTROL-DB' doesn't exist")
      (init-dbs force-init))
    (debug-msg "dbs initialized")
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

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE
