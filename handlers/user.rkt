#lang racket
(require threading
         forms
         net/url-structs
         web-server/dispatch
         web-server/http/xexpr
         web-server/http/cookie
         web-server/http/redirect
         web-server/http/request-structs
         web-server/http/response-structs
         "../config.rkt"
         "../util/db.rkt"
         "../util/misc.rkt"
         "../util/session.rkt"
         "../i18n/utils.rkt"
         "../models/user.rkt"
         "../views/user.rkt")

(provide
 (contract-out
  #:∃ user-page
  [user-servlet (-> request? response?)]
  [user-url     (-> user-page any/c ... string?)]
  [login-page   user-page]
  [signout-page user-page]))

(define-values (user-servlet user-url)
  (dispatch-rules
   [("login") #:method (or "get" "post") login-page]
   [("signup") #:method (or "get" "post") signup-page]
   [("signout") #:method "post" signout-page]))

(define login-form
  (form* ([username (ensure binding/text (required) (shorter-than 30))]
          [password (ensure binding/text (required) (shorter-than 50))])
    (cons username (string->bytes/utf-8 password))))

(define (login-page req)
  (define redirect-url
    (or (request-query-param req 'request-url) "/"))

  ;; if the user is already logged in, we just redirect them
  (if (current-user)
      (redirect-to redirect-url see-other)
      (match (form-run login-form req)
        [`(passed (,username . ,password) ,render-widget)
         (define maybe-user (check-username-password username password))
         (if maybe-user
             (redirect-to
              redirect-url see-other          
              #:headers
              (list
               (cookie->header
                (create-session-cookie
                 #:user-id (user-id maybe-user)
                 #:expires-at (seconds->date (+ (current-seconds)
                                                (years->seconds 7)))
                 #:ip-address (request-client-ip req)
                 #:user-agent (and~> req request-headers/raw
                                     (headers-assq* #"user-agent" _)
                                     header-value
                                     (bytes->string/utf-8 #\�))))))
             (response/xexpr (render-login-form render-widget (list ($ invalid-username-or-password)))))]
        [(list _ _ render-widget)
         (response/xexpr (render-login-form render-widget))])))

(define signup-password
  (form* ([password (ensure binding/text (required) (shorter-than 50))]
          [password-confirmation (ensure binding/text (required) (shorter-than 50))])
    (if (equal? password password-confirmation)
        (ok password)
        (err `((password-confirmation . ,($ passwords-do-not-match)))))))

(define signup-form
  (form* ([username (ensure binding/text (required) (shorter-than 30))]
          [password signup-password])
    (cons username (string->bytes/utf-8 password))))

(define (signup-page req)
  (when (not (signup-enabled))
    (notify-signup-disabled))

  (define redirect-url
    (or (request-query-param req 'request-url) "/"))

  (if (current-user)
      (redirect-to redirect-url see-other)
      (match (form-run signup-form req)
        [`(passed (,username . ,password) ,render-widget)
         (with-handlers ([exn:fail:sql:unique-username-violation?
                          (λ (e)
                            (response/xexpr
                             (render-signup-form render-widget
                                                 (list ($ user-already-exists)))))])
           (call-with-transaction
            (λ () (create-user! username password)))
           (redirect-to (url-with-params (user-url login-page)
                                         `((redirect-url . ,redirect-url))) see-other))]
        [(list _ _ render-widget)
         (response/xexpr (render-signup-form render-widget))])))

(define (signout-page req)
  ;; we probably don't want to do something like challenge the user to authenticate
  ;; so that we can immediately sign them out...
  (and~> req get-session-from-request
         ((λ~> delete-session! thunk call-with-transaction)))
  (define redirect-url (request-query-param req 'redirect-url))
  (redirect-to (or redirect-url "/") see-other))
