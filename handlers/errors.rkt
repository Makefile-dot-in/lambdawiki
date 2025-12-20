#lang racket
(require racket/exn

         threading
         web-server/http/xexpr
         web-server/http/request-structs
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         web-server/dispatchers/dispatch

         "../session.rkt"
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
  [(struct exn:fail:unauthenticated _)
   401 "Unauthenticated" "You need to be authenticated to access this resource."]
  [_ 500 "Internal server error" "Your request could not be processed due to an internal server error."])

(define (http-error-responder _url err)
  (when (exn:fail? err)
    ((error-display-handler) (exn->string err) err))
  (let-values ([(ret short description) (exn-values err)])
    (response/xexpr (error-view short description)
                    #:code ret)))

(define (error-dispatcher inner)
  (λ (conn req)
    (let ([handler (λ~> (http-error-responder (request-uri req) _)
                        (λ (req) _) lift:make
                        (_ conn req))])
      (with-handlers ([exn:fail? handler]
                      [exn:dispatcher? handler])
        (inner conn req)))))

