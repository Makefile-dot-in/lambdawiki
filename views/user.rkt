#lang racket
(require xml
         forms

         "../i18n/utils.rkt"
         "../config.rkt"
         "base.rkt")

(provide
 (contract-out
  [render-login-form (->* [widget-renderer/c]
                          [(or/c #f (listof string?))]
                          xexpr?)]
  [render-signup-form (->* [widget-renderer/c]
                           [(or/c #f (listof string?))]
                           xexpr?)]))

(define (render-login-form render-widget [errors #f])
  (base-template
   ($ login-title)
   `(main
     ([id "login-page"])
     (h1 ,($ login-header ,(wiki-name)))
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

(define (render-signup-form render-widget [errors #f])
  (define render-password-subwidget
    (widget-namespace "password" render-widget))
  
  (base-template
   ($ login-title)
   `(main
     ([id "signup-page"])
     (h1 ,($ signup-header ,(wiki-name)))
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
             ,(render-password-subwidget "password" (widget-password)))
      ,@(render-password-subwidget "password" (widget-errors))
      (label ,($ signup-password-confirm)
             ,(render-password-subwidget "password-confirmation" (widget-password)))
      ,@(render-password-subwidget "password-confirmation" (widget-errors))
      (input ([type "submit"] [value ,($ signup-submit)]))))))
