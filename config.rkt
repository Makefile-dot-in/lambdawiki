#lang racket
(provide
 (contract-out
  [with-config (-> port? (-> any) any)])
 wiki-name default-locale useful-links)

(define/contract wiki-name (parameter/c string?)
  (make-parameter "Lambdawiki"))

(define/contract default-locale (parameter/c string?)
  (make-parameter "en"))

(define/contract useful-links (parameter/c (listof (cons/c string? string?)))
  (make-parameter '(("Home" . "/"))))

(define (with-config port f)
  (parameterize ()
    (for ([entry (in-port read port)])
      (match entry
        [`(wiki-name ,name)        (wiki-name name)]
        [`(default-locale ,locale) (default-locale locale)]
        [`(useful-links ,links)    (useful-links links)]
        [_                         (error 'config "Unknown form: ~a" entry)]))
    (f)))


