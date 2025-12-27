#lang racket
(require threading
         "session.rkt"
         "../util/snowflake.rkt"
         "../models/user.rkt"
         "../models/permissions.rkt")

(provide
 (struct-out exn:fail:unauthorized)
 (contract-out
  [check-authorization-for-article (-> symbol? snowflake? any)]
  [check-authorization-for-user (-> symbol? snowflake? any)]
  [check-authorization-for-class (-> symbol? snowflake? any)]
  [register-article-permission! (-> symbol? string? any)]
  [register-user-permission! (-> symbol? string? any)]))

(struct exn:fail:unauthorized exn:fail (permission-kind permission subject))

(define (unauthorized kind permission subject)
  (raise (exn:fail:unauthorized
          (format "unauthorized for ~a ~a: no ~a permission"
                  kind subject permission)
          (current-continuation-marks)
          kind permission subject)))

(define (check-authorization-for-article perm article-id)
  (when (not (has-article-permission? perm (and~> (current-user) user-id) article-id))
    (unauthorized 'article perm article-id)))

(define (check-authorization-for-user perm user-id)
  (when (not (has-user-permission? perm (and~> (current-user) user-id) user-id))
    (unauthorized 'user perm user-id)))

(define (check-authorization-for-class perm class-id)
  (when (not (has-permission-for-class? perm (and~> (current-user) user-id) class-id))
    (unauthorized 'article-class perm class-id)))

(define article-permissions (make-hash))
(define (register-article-permission! symbol description)
  (hash-set! article-permissions symbol description))

(define user-permissions (make-hash))
(define (register-user-permission! symbol description)
  (hash-set! user-permissions symbol description))
