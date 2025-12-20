#lang racket
(require sql
         threading
         crypto
         db/util/datetime

         racket/random
         
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (struct-out user)
 (contract-out
  [start-user-services (-> any)]
  [session-id? flat-contract?]
  [create-session! (->* [#:user-id snowflake?]
                        [#:expires-at date?
                         #:created-at date?
                         #:ip-address string?
                         #:user-agent string?]
                        session-id?)]
  [get-user-from-session (-> session-id? (or/c #f user?))]
  [check-username-password (-> string? bytes? (or/c #f user?))]
  [delete-session! (-> session-id? any)]))

(define session-id? bytes?)
(define (start-user-services)
  (thread session-gc))

;; removes expired sessions hourly forever
(define (session-gc)
  (call-with-transaction
   remove-expired-sessions)
  (sleep 3600)
  (session-gc))

(define (remove-expired-sessions)
  (query-exec
   (delete #:from sessions
           #:where (expires_at . < . (now)))))

(define (create-session! #:user-id user-id
                         #:expires-at [expires-at #f]
                         #:created-at [created-at #f]
                         #:ip-address [ip-address #f]
                         #:user-agent [user-agent #f])
  (let ([session-id (crypto-random-bytes 14)])
    (query-exec
     (insert #:into sessions
             #:set
             [session_id ,session-id]
             [user_id ,user-id]
             [expires_at ,(and~> expires-at srfi-date->sql-timestamp-tz)]
             [created_at ,(and~> created-at srfi-date->sql-timestamp-tz)]
             [ip_address ,ip-address]
             [user_agent ,user-agent]))
    session-id))

(struct user (id username))
(define/match (vector->user _v)
  [((vector id username))
   (user id username)])

(define (get-user-from-session session-id)
  (and~>
   (query-maybe-row
    (select
     #:from (left-join sessions users
                       #:on (= sessions.user_id users.id))
     #:where (= sessions.session_id ,session-id)
     #:values id username))
   vector->user))


(define (check-username-password username password)
  (match (query-maybe-row
          (select
           #:from users
           #:where (= username ,username)
           #:values id pwhash))
    [(vector id (app bytes->string/utf-8 pwhash))
     (and (pwhash-verify #f password pwhash)
          (user id username))]
    [#f #f]))

(define (delete-session! session-id)
  (query-exec
   (delete #:from sessions
           #:where (= session_id ,session-id))))
