#lang racket
(provide
 (except-out (all-from-out racket) #%module-begin)
 (rename-out [module-begin #%module-begin]))

(define-syntax module-begin
  (syntax-rules (prelude)
    [(_ (prelude forms ...) (pattern-car pattern-cdr part ...) ...)
     (#%module-begin
      (provide localize)
      forms ...
      (define/match (localize val)
        [(`(pattern-car . pattern-cdr)) (string-append (~a part) ...)] ...))]
    [(_ (pattern-car pattern-cdr part ...) ...)
     (module-begin (prelude) (pattern-car pattern-cdr part ...) ...)]))
