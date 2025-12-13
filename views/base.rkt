#lang racket
(require xml
         "../config.rkt")

(provide
 (contract-out
  [base-template (string? xexpr? . -> . xexpr?)]))

;; Basic template for displaying the view
(define (base-template title content)
  `(html
    (head (title ,(format "~a : ~a" title (wiki-name))))
    (body
     (aside ([id "sidebar"])
       (header (wiki-name))
       
       (aside ([id "links"])
         (ul (li (a ([href "/"]) "Home"))))
       
       (section ([id "search"])
         (form ([method "GET"] [action "/search"])
               (label ([for "search-box"]) "Search:")
               (input ([type "text"] [name "q"] [id "search-box"]))
               (input ([type "submit"] [value "go"])))))

     ,content)))
