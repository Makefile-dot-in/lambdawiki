#lang racket
(require racket/runtime-path
         "registry.rkt")

(provide
 (contract-out
  [load-renderers! (-> any)])

 render-content-type
 content-type-mime)

(define-runtime-path module-path ".")
(define (load-renderers!)
  (for ([d (in-directory module-path)]
        #:when (and (string-suffix? (path->string d) ".rkt")
                    (not (member d (list "registry.rkt" "main.rkt"))))) 
    (dynamic-require d #f)))
