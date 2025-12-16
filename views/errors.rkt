#lang racket
(require xml
         "base.rkt")
(provide
 (contract-out
  [error-view (-> string? string? xexpr?)]))

(define (error-view short description)
  (base-template
   short
   `(main ([class "error-page"])
     (div ([class "summary"]) ,short)
     (div ([class "elaboration"]) ,description))))
   
