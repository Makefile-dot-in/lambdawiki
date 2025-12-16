#lang racket
(require xml
         "../config.rkt"
         "../i18n/utils.rkt")

(provide
 (contract-out
  [base-template (string? xexpr? . -> . xexpr?)]))

;; Basic template for displaying the view
(define (base-template title content)
  `(html
    (head (title ,(format "~a : ~a" title (wiki-name)))
          (link ([rel "stylesheet"] [href "/static/style.css"])))
    (body
     (aside ([id "sidebar"])
       (header ,(wiki-name))
       
       (aside ([id "links"])
         (div ,(format "~a:" ($ useful-links)))
         (ul ,@(for/list ([l (useful-links)])
                 `(li (a ([href ,(cdr l)]) ,(car l))))))
       
       (section ([id "search"])
         (form ([method "GET"] [action "/search"])
               (label ([for "search-box"]) ,(format "~a:" ($ search-label)))
               (input ([type "text"] [name "q"] [id "search-box"]))
               (input ([type "submit"] [value ,($ search-go)])))))

     (div ([id "content"])
          ,content))))
