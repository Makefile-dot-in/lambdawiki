#lang racket
(require xml
         "../config.rkt"
         "../models/user.rkt"
         "../util/session.rkt"
         "../util/misc.rkt"
         "../i18n/utils.rkt")

(provide
 (contract-out
  [base-template (->* [string? xexpr?]
                      [#:search-query string?]
                      xexpr?)]
  [generate-table (->* [(listof xexpr?) (listof (listof xexpr?))]
                       [#:id (or/c #f string?)]
                       xexpr?)]
  [pagination-controls (-> #:limit exact-integer?
                           #:offset exact-integer?
                           #:count (or/c #f exact-integer?)
                           #:per-page-label string?
                           (listof xexpr?))]))

;; Basic template for displaying the view
(define (base-template title content #:search-query [search-query ""])
  `(html
    (head (title ,(format "~a : ~a" title (wiki-name)))
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [href "/static/style.css"])))
    (body
     (aside ([id "sidebar"])
       (header ,(wiki-name))
       
       (section ([id "links"])
         (div ,(format "~a:" ($ useful-links)))
         (ul ,@(for/list ([l (useful-links)])
                 `(li (a ([href ,(cdr l)]) ,(car l))))))

       (section ([id "create-article"])
         (a ([href "/new-article"]) ,($ article-create-sidelink)))
       
       (section ([id "user-info"])
         ,(match (current-user)
            [(user _ name)
             `(div ,($ login-status ,name) " "
                   (form ([method "POST"] [action ,(url-with-params "/signout" `((redirect-to . ,(current-url))))])
                         (input ([type "submit"] [class "link-input"] [value ,($ signout-link)]))))]
            [#f
             `(div ,($ login-status #f) " "
                   ,@(if (not (signup-enabled)) null
                         `((a ([href ,(url-with-params "/signup" `((redirect-to . ,(current-url))))])
                              ,($ signup-link))
                           " "))
                   (a ([href ,(url-with-params "/login" `((redirect-to . ,(current-url))))])
                      ,($ login-link)))]))
       
       (section ([id "search"])
         (form ([method "GET"] [action "/search"])
               (label ([for "search-box"]) ,(format "~a:" ($ search-label)))
               (input ([type "text"]
                       [name "q"]
                       [id "search-box"]
                       [value ,search-query]))
               (input ([type "submit"] [value ,($ search-go)])))))

     (div ([id "content"])
          ,content))))

(define (generate-table headers rows #:id [id #f])
  `(table ,@(if (not id) null `(((id ,headers))))
    (thead
     (tr ,@(map (curry list 'th) headers)))
    (tbody
     ,@(for/list ([r rows])
         `(tr ,@(map (curry list 'td) r))))))

(define (pagination-controls
         #:limit limit
         #:offset offset
         #:count count
         #:per-page-label label)
  `((form
     ([method "get"])
     (label
      ,(format "~a:" label)
      (select
          ([name "limit"])
        ,@(for/list ([l '(25 50 100)])
            (define lstr (number->string l))
            (if (equal? l limit)
                `(option ([value ,lstr] [selected "selected"]) ,lstr)
                `(option ([value ,lstr]) ,lstr)))))

     (input ([type "submit"]
             [value ,($ page-limit-submit)]))

     (div 
      ,@(if (equal? offset 0) null
            `((a ([href
                   ,(url-with-params
                     ""
                     `((offset . ,(number->string (max 0 (- limit offset))))
                       (limit . ,(number->string limit))))])
                 ,(format "[~a]" ($ page-prev)))))
      

      ,@(if (or (not count) ((+ limit offset) . >= . count)) null
            `((a ([href
                   ,(url-with-params
                     ""
                     `((offset . ,(number->string (max 0 (+ limit offset))))
                       (limit . ,(number->string limit))))])
                 ,(format "[~a]" ($ page-next)))))))))
