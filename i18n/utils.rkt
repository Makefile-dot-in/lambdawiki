#lang racket
(require memo
         threading
         "../config.rkt"
         racket/runtime-path
         web-server/http/request-structs
         web-server/dispatchers/dispatch)

(provide
 $
 module-path
 (contract-out
  [formatter/c       contract?]
  [languages         (listof (cons/c string? formatter/c))]
  [locale-dispatcher (-> dispatcher/c dispatcher/c)]
  [format-datum      formatter/c]))


(define formatter/c (-> any/c string))
(define-runtime-path module-path ".")

;; an alist of languages sorted by length
(define languages
  (~> (for/list ([f (in-directory module-path)]
                 #:do [(define m
                         (regexp-match #rx"^(..)\\.rkt$"
                                       (file-name-from-path f)))]
                 #:when m)
        (match-let* ([(list _ lang) m]
                     [localize (dynamic-require f 'localize)])
          (cons lang localize)))
      (sort < #:key (compose length car))))

;; returns the entry for the language that matches lang the best
;; where best is being the longest prefix of lang-spec
(define/memoize (most-appropriate-localization lang-spec)
  (for/first ([pair languages]
              #:do [(define lang (car pair))]
              #:when (string-prefix? lang-spec lang))
    pair))

(define (format-datum datum)
  (~> (current-locale)
      (assoc languages)
      (cdr)
      (_ datum)))

(define-syntax-rule ($ . sexp)
  (format-datum `sexp))

(define (parse-accept-language s)
  (~> (string-split s #rx"\\s*,\\s*")
      (filter-map (match-λ
            [(regexp #rx"^([a-zA-Z-]+);q=([0-9.]+)$" (list _ lang q))
             (cons lang (or (string->number q 10 'number-or-false) 0))]
            [(regexp #rx"^[a-zA-Z-]+$" (list lang))
             (cons lang 1)]
            [_ #f])
           _)
      (sort >= #:key cdr)
      (map car _))) ; CL reference !

(define (find-best-language req)
  (or (and~> (request-headers/raw req)
             (headers-assq* #"accept-language" _)
             (header-value)
             (bytes->string/utf-8 #\�)
             (parse-accept-language)
             (filter-map most-appropriate-localization _)
             (car)
             (car))
      (default-locale)))

(define (locale-dispatcher inner)
  (λ (conn req)
    (parameterize ([current-locale (find-best-language req)])
      (inner conn req))))
