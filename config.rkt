#lang racket
(provide
 (contract-out
  [with-config (-> port? (-> any) any)])
 wiki-name default-locale)

(define/contract wiki-name (parameter/c string?)
  (make-parameter "Lambdawiki"))

(define/contract default-locale (parameter/c string?)
  (make-parameter "en"))

(define (with-config port f)
  (parameterize ()
    (for ([entry (in-port read port)])
      (match entry
        [`(wiki-name ,name)        (wiki-name name)]
        [`(default-locale ,locale) (default-locale locale)]
        [_                         (error 'config "Unknown form: ~a" entry)]))
    (f)))


