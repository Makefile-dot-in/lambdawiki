#lang racket
(require (prefix-in db: db)
         (only-in
          db
          virtual-connection connection-pool
          dsn-connect connection?
          sql-null->false
          sql-null
          sql-null?
          exn:fail:sql
          exn:fail:sql?
          exn:fail:sql-info
          exn:fail:sql-sqlstate)
         threading
         "../config.rkt")

(provide
 ;; database initialization
 (contract-out
  [current-connection (parameter/c (or/c #f connection?))]
  [create-connection (-> connection?)]
  [with-connection-from-config (-> (-> any/c) any/c)]
  [is-unique-name-violation? (-> string? any/c boolean?)])

 ;; implicit versions of db's functions
 query-exec
 query-rows

 query-list
 query-row
 query-maybe-row
 query-maybe-value
 in-query
 call-with-transaction

 ;; re-export from db
 connection?
 sql-null->false
 sql-null
 sql-null?
 exn:fail:sql
 exn:fail:sql?
 exn:fail:sql-sqlstate
 exn:fail:sql-info)

(define current-connection
  (make-parameter #f))

(define unique-constraint-violation "23505")

(define (is-unique-name-violation? constraint-name e)
  (and (exn:fail:sql? e)
       (equal? (exn:fail:sql-sqlstate e) unique-constraint-violation)
       (equal? (and~> (assoc 'constraint (exn:fail:sql-info e)) cdr)
               constraint-name)))

(define (create-connection)
  (virtual-connection
   (connection-pool
    (λ ()
      (dsn-connect (active-dsn))))))

(define (with-connection-from-config f)
  (parameterize ([current-connection (create-connection)]) (f)))

;; simplifies the defining of database query functions that implicitly use (current-database)
(define-syntax-rule (define-implicit-specializations [implicit source] ...)
  (begin
    (define implicit
      (make-keyword-procedure
       (λ (kw kw-args . args)
         (keyword-apply source kw kw-args
                        (current-connection)
                        args))))
    ...))

(define-implicit-specializations
  [query-exec            db:query-exec]
  [query-rows            db:query-rows]
  [query-list            db:query-list]
  [query-row             db:query-row]
  [query-maybe-row       db:query-maybe-row]
  [query-maybe-value     db:query-maybe-value]
  [in-query              db:in-query]
  [call-with-transaction db:call-with-transaction])

