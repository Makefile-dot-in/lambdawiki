#lang racket
(require threading
         net/url
         "snowflake.rkt"
         web-server/http/request-structs
         web-server/dispatchers/dispatch)

(provide
 (struct-out exn:fail:article-not-found)
 (struct-out exn:fail:revision-not-found)
 (struct-out exn:fail:signup-disabled)
 (contract-out
  [hours->seconds (-> number? number?)]
  [days->seconds  (-> number? number?)]
  [years->seconds (-> number? number?)]
  [current-url (parameter/c (or/c #f string?))]
  [not-found (-> string? any)]
  [revision-not-found (-> snowflake? any)]
  [contextualizing-dispatcher (-> dispatcher/c dispatcher/c)]
  [request-query-param (-> request? symbol? (or/c string? #f))]
  [url-with-params (-> string? (listof (cons/c symbol? string?)) string?)]
  [notify-signup-disabled (-> any)]))

(define (hours->seconds h) (* h 3600))
(define (days->seconds  d) (hours->seconds (* d 24)))
(define (years->seconds y) (days->seconds  (* y 365)))

(define current-url
  (make-parameter #f))

(struct exn:fail:article-not-found exn:fail (article-name))
(struct exn:fail:revision-not-found exn:fail (revision-id))
(struct exn:fail:signup-disabled exn:fail ())

(define (notify-signup-disabled)
  (raise (exn:fail:signup-disabled "signup disabled" (current-continuation-marks))))

(define (not-found name)
  (raise (exn:fail:article-not-found
          (format "article ~a not found" name)
          (current-continuation-marks)
          name)))

(define (revision-not-found id)
  (raise (exn:fail:revision-not-found)
         (format "revision ~a not found" id)
         (current-continuation-marks)
         id))

;; turns current-url into a parameter to allow embedding redirects in views/base
(define (contextualizing-dispatcher inner)
  (λ (conn req)
    (parameterize ([current-url (url->string (request-uri req))])
      (inner conn req))))

(define (request-query-param req param)
  (and~> req request-uri url-query (assoc param _) cdr))

;; adds the given query paramer to the url
(define (url-with-params old-url params)
  (let ([u (string->url old-url)])
    (url->string
     (struct-copy url u [query (append (url-query u) params)]))))
