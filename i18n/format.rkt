#lang racket
(provide
 (except-out (all-from-out racket) #%module-begin)
 (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin [pattern-car pattern-cdr part ...] ...)
  (#%module-begin
   (provide localize)
   (define/match (localize val)
     [(`(pattern-car . pattern-cdr)) (string-append (~a part) ...)] ...)))
