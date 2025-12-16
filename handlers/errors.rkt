#lang racket
(require web-server/http/xexpr
         web-server/servlet-dispatch
         web-server/dispatchers/dispatch
         "../views/errors.rkt")

(provide
 (contract-out
  [error-dispatcher (-> dispatcher/c dispatcher/c)]))

(define-syntax-rule (define-exn-values fn [pat ret short description] ...)
  (define/match (fn e)
    [(pat) (values ret short description)] ...))

(define-exn-values exn-values
  [(struct exn:dispatcher _)
   404 "Not found" "The requested resource was not found."]
  [_ 500 "Internal server error" "Your request could not be processed due to an internal server error."])

(define (http-error-handler err _req)
  (let-values ([(ret short description) (exn-values err)])
    (response/xexpr (error-view short description)
                    #:code ret)))

(define (error-dispatcher inner)
  (λ (conn req)
    (with-handlers ([(or/c exn:fail? exn:dispatcher?)
                     (λ (e) ((dispatch/servlet (curry http-error-handler e)) conn req))])
      (inner conn req))))

