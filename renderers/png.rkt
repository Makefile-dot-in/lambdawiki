#lang racket
(require racket/lazy-require
         "registry.rkt")
(lazy-require ["../handlers/article.rkt" (wiki-by-id)])

(define (renderer article-id _)
  `((img ([id "main-image"] [src ,(wiki-by-id article-id "raw")]))))

(register-content-type! "png" "PNG" renderer
                        #:binary #t
                        #:mime-type #"image/png")
