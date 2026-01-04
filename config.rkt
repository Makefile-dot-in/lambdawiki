#lang racket
(require db)
(provide
 (contract-out
  [with-config (-> port? (-> any) any)])
 wiki-name default-locale useful-links active-dsn http-port
 main-page signup-enabled)

(define/contract wiki-name (parameter/c string?)
  (make-parameter "Lambdawiki"))

(define/contract default-locale (parameter/c string?)
  (make-parameter "en"))

(define/contract useful-links (parameter/c (listof (cons/c string? string?)))
  (make-parameter '(("Home" . "/")
                    ("All articles" . "/all-articles"))))

(define/contract active-dsn (parameter/c (or/c #f data-source?))
  (make-parameter #f))

(define/contract http-port (parameter/c exact-integer?)
  (make-parameter 8080))

(define/contract main-page (parameter/c string?)
  (make-parameter "Main Page"))

(define/contract signup-enabled (parameter/c boolean?)
  (make-parameter #f))

(define (with-config port f)
  (parameterize ()
    (for ([entry (in-port read port)])
      (match entry
        [`(wiki-name ,name)         (wiki-name name)]
        [`(default-locale ,locale)  (default-locale locale)]
        [`(useful-links ,links)     (useful-links links)]
        [`(data-source ,src)        (active-dsn (apply data-source src))]
        [`(port ,port)              (http-port port)]
        [`(main-page ,page)         (main-page page)]
        [`(signup-enabled ,p)       (signup-enabled p)]
        [_                          (error 'config "Unknown form: ~a" entry)]))
    (f)))


