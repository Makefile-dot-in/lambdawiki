#lang racket
(require xml
         forms

         "../i18n/utils.rkt"
         "base.rkt")

(provide
 (contract-out
  [render-login-form (->* [widget-renderer/c]
                          [(or/c #f (listof string?))]
                          xexpr?)]))

(define (render-login-form render-widget [errors #f])
  (base-template
   ($ login-title)
   `(main
     ([id "login-page"])
     (form
      ([method "post"])
      ,@(if errors
            `((ul ([class "error"])
                  ,@(for/list ([e errors]) `(li ,e))))
            null)
      (label ,($ login-username)
             ,(render-widget "username" (widget-text)))
      ,@(render-widget "username" (widget-errors))
      (label ,($ login-password)
             ,(render-widget "password" (widget-password)))
      ,@(render-widget "password" (widget-errors))
      (input ([type "submit"] [value ,($ login-submit)]))))))
