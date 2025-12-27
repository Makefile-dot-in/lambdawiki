#lang racket/base
(require scribble/reader
         (for-syntax racket/base))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out scribble/reader)
         (rename-out [module-begin #%module-begin])
         bold italic section subsection ref
         table link)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ (arg ...))
     (with-syntax ([output (datum->syntax stx 'output)])
       #'(#%module-begin
          (provide output)
          (define output
            (list arg ...))))]))

(define-syntax-rule (quoted-forms arg ...)
  (begin
    (define (arg . args) (cons 'arg args)) ...))

(quoted-forms bold italic section subsection ref)

(define (table #:alignments [alignments null]
               #:headers [headers null]
               . rest)
  `(table ,alignments ,headers . ,rest))

(define (link target . appearance)
  `(link ,target ,(if (null? appearance) target appearance)))
