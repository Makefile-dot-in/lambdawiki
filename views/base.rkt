#lang racket
(require xml
         "../config.rkt"
         "../models/user.rkt"
         "../util/session.rkt"
         "../util/misc.rkt"
         "../i18n/utils.rkt")

(provide
 (contract-out
  [base-template (string? xexpr? . -> . xexpr?)]))

;; Basic template for displaying the view
(define (base-template title content)
  `(html
    (head (title ,(format "~a : ~a" title (wiki-name)))
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [href "/static/style.css"])))
    (body
     (aside ([id "sidebar"])
       (header ,(wiki-name))
       
       (aside ([id "links"])
         (div ,(format "~a:" ($ useful-links)))
         (ul ,@(for/list ([l (useful-links)])
                 `(li (a ([href ,(cdr l)]) ,(car l))))))

       (aside ([id "user-info"])
         ,(match (current-user)
            [(user _ name)
             `(div ,($ login-status ,name) " "
                   (form ([method "POST"] [action ,(url-with-params "/signout" `((redirect-to . ,(current-url))))])
                         (input ([type "submit"] [class "link-input"] [value ,($ signout-link)]))))]
            [#f
             `(div ,($ login-status #f) " "
                   (a ([href ,(url-with-params "/login" `((redirect-to . ,(current-url))))])
                      ,($ login-link)))]))
       
       (section ([id "search"])
         (form ([method "GET"] [action "/search"])
               (label ([for "search-box"]) ,(format "~a:" ($ search-label)))
               (input ([type "text"] [name "q"] [id "search-box"]))
               (input ([type "submit"] [value ,($ search-go)])))))

     (div ([id "content"])
          ,content))))
