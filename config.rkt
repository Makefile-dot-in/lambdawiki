#lang racket
(require db)
(provide
 (contract-out
  [with-config (-> port? (-> any) any)])
 wiki-name default-locale useful-links active-dsn)

(define/contract wiki-name (parameter/c string?)
  (make-parameter "Lambdawiki"))

(define/contract default-locale (parameter/c string?)
  (make-parameter "en"))

(define/contract useful-links (parameter/c (listof (cons/c string? string?)))
  (make-parameter '(("Home" . "/"))))

(define/contract active-dsn (parameter/c (or/c #f data-source?))
  (make-parameter #f))

(define (with-config port f)
  (parameterize ()
    (for ([entry (in-port read port)])
      (match entry
        [`(wiki-name ,name)        (wiki-name name)]
        [`(default-locale ,locale) (default-locale locale)]
        [`(useful-links ,links)    (useful-links links)]
        [`(data-source ,src)       (active-dsn (apply data-source src))]
        [_                         (error 'config "Unknown form: ~a" entry)]))
    (f)))


