#lang racket
(require threading
         net/base64
         net/cookies/server
         web-server/dispatchers/dispatch
         web-server/http/request-structs
         web-server/http/cookie-parse

         racket/date

         "db.rkt"
         "snowflake.rkt"
         "../models/user.rkt")

(provide
 (struct-out
  exn:fail:unauthenticated)
 (contract-out
  [current-user          (parameter/c (or/c #f user?))]
  [session-dispatcher    (-> dispatcher/c dispatcher/c)]
  [create-session-cookie (->* [#:user-id snowflake?]
                              [#:expires-at date?
                               #:ip-address string?
                               #:user-agent string?]
                              cookie?)]
  [get-session-from-request (-> request? session-id?)]))

(define current-user
  (make-parameter #f))

(define (get-session-from-request req)
  (and~> req
         request-cookies
         (findf
          (match-λ
           [(struct* client-cookie ([name "lw_session_id"])) #t]
           [_ #f])
          _)
         client-cookie-value
         string->bytes/utf-8
         base64-decode))

(define (get-user-from-request req)
  (and~> req
         get-session-from-request
         get-user-from-session))


(define (hours->seconds n)
  (* n 3600))

;; create a session cookie. if expires-at is not provided, then it will only last for one session.
(define (create-session-cookie #:user-id user-id
                               #:expires-at [expires-at #f]
                               #:ip-address [ip-address #f]
                               #:user-agent [user-agent #f])
  (~> (call-with-transaction
       (λ () (create-session! #:user-id user-id
                              ;; we assume that no session will last longer than 2 days
                              #:expires-at (or expires-at
                                               (seconds->date (+ (current-seconds)
                                                                 (hours->seconds 48))))
                              #:created-at (current-date)
                              #:ip-address ip-address
                              #:user-agent user-agent)))
      (base64-encode "")
      (make-cookie "lw_session_id" _
                   #:expires expires-at)))

(define (session-dispatcher inner)
  (λ (conn req)
    (parameterize ([current-user (get-user-from-request req)])
      (inner conn req))))
