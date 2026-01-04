#lang racket
(require racket/exn

         threading
         web-server/http/xexpr
         web-server/http/request-structs
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         web-server/dispatchers/dispatch

         "../i18n/utils.rkt"
         "../util/permissions.rkt"
         "../util/misc.rkt"
         "../views/errors.rkt")

(provide
 (contract-out
  [error-dispatcher (-> dispatcher/c dispatcher/c)]))

(define-syntax-rule (define-exn-values fn [pat ret sym] ...)
  (define (fn e)
    (let-values ([(code symb)
                  (match e
                    [pat (values ret 'sym)] ...)])
      (values code
              ($ error-short ,symb)
              ($ error-description ,symb)))))

(define-exn-values exn-values
  [(struct exn:dispatcher _) 404 no-handler]
  [(struct exn:fail:article-not-found _) 404 article-not-found]
  [(struct exn:fail:revision-not-found _) 404 revision-not-found]
  [(struct exn:fail:unauthorized _) 403 unauthorized]
  [(struct exn:fail:signup-disabled _) 403 signup-disabled]
  [_ 500 internal-error])

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

