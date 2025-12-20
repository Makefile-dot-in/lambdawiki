#lang racket
(require threading
         net/url
         web-server/http/request-structs
         web-server/dispatchers/dispatch)

(provide
 (contract-out
  [hours->seconds (-> number? number?)]
  [days->seconds  (-> number? number?)]
  [years->seconds (-> number? number?)]
  [current-url (parameter/c (or/c #f string?))]
  [contextualizing-dispatcher (-> dispatcher/c dispatcher/c)]
  [request-query-param (-> request? symbol? (or/c string? #f))]
  [url-with-params (-> string? (listof (cons/c symbol? string?)) string?)]))

(define (hours->seconds h) (* h 3600))
(define (days->seconds  d) (hours->seconds (* d 24)))
(define (years->seconds y) (days->seconds  (* y 365)))

(define current-url
  (make-parameter #f))

;; turns current-url into a parameter to allow embedding redirects in views/base
(define (contextualizing-dispatcher inner)
  (λ (conn req)
    (parameterize ([current-url (url->string (request-uri req))])
      (inner conn req))))

(define (request-query-param req param)
  (and~> req request-uri url-query (assoc param _)))

;; adds the given query paramer to the url
(define (url-with-params old-url params)
  (let ([u (string->url old-url)])
    (url->string
     (struct-copy url u [query (append (url-query u) params)]))))
