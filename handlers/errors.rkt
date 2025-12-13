#lang racket
(require web-server
         web-server/dispatchers/dispatch
         "../views/errors.rkt")

(provide
 (contract-out
  [error-dispatcher (-> dispatcher?)]))

(define-syntax-rule (define-exn-values fn [pat ret short description] ...)
  (define/match (fn e)
    [(pat) (values ret short description)] ...))

(define-exn-values exn-values
  [(struct exn:dispatcher _)
   404 "Not found" "The requested resource was not found."])

(define (error-dispatcher inner c req)
  ())

