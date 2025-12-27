#lang racket
(require threading
         racket/sandbox
         racket/list/grouping
         racket/runtime-path
         racket/lazy-require
         (prefix-in reader: scribble/reader)
         "registry.rkt"
         "../i18n/utils.rkt")

(lazy-require ["../handlers/article.rkt" (url-to-article)])

(define (append-refs refpair ref)
  (set-mcar! refpair (+ 1 (mcar refpair)))
  (set-mcdr! refpair (cons ref (mcdr refpair)))
  (mcar refpair))

(struct secent (number kind content))

(define (append-section secpair section)
  (set-mcar! secpair (+ 1 (mcar secpair)))
  (set-mcdr! secpair (cons (secent (mcar secpair) 'section section)
                           (mcdr secpair)))
  (mcar secpair))

(define (append-subsection secpair section)
  (set-mcar! secpair (+ 1 (mcar secpair)))
  (set-mcdr! secpair (cons (secent (mcar secpair) 'subsection section)
                           (mcdr secpair)))
  (mcar secpair))

(define (generate-row refs secs elem alignments data)
  `(tr ,@(for/list ([cell data]
                    [alignment (in-sequences alignments (in-cycle 'left))])
           `(,elem ([class ,(case alignment
                              ['left "align-left"]
                              ['right "align-right"])])
                   ,@(map (curry render-wikiscribble-element refs secs) cell)))))

(define (render-wikiscribble-element refs secs f)
  (define recurse (curry render-wikiscribble-elements refs secs))
  (match f
    [`(bold   . ,r)
     `(b . ,(recurse r))]
    [`(italic . ,r)
     `(i . ,(recurse r))]
    [`(section . ,r)
     (append-section secs r)
     `(h2 . ,(recurse r))]
    [`(subsubsection . ,r)
     (append-subsection refs r)
     `(h3 . ,(recurse r))]
    [`(ref . ,r)
     (define n (append-refs refs r))
     `(a ([href ,(format "#ref-~a" n) class "ref"])
         ,(format "[~a]" n))]
    [`(table ,alignments ,headers . ,rows)
     `(table
       ,@(if (null? headers) null
             `((thead ,@(generate-row refs 'th alignments headers))))

       (tbody
        ,@(for/list ([row rows])
            (generate-row refs 'td alignments row))))]
    [`(link ,target . ,appearance)
     `(a ([href ,(url-to-article target)]) ,@(recurse appearance))]
    [(? string?) f]))

(define (render-wikiscribble-elements refs secs lst)
  (map (curry render-wikiscribble-element refs secs) lst))

(define (add-refs refs forms)
  (append
   forms
   `((section ,($ reference-title))
     (ul
      ,@(for/list ([(ref i) (in-indexed (reverse refs))])
          `(li (a ([id ,(format "ref-~a" i)]))
               (cite ,@(map (curry render-wikiscribble-element (mcons 0 null) (mcons 0 null)) ref))))))))

(define (table-of-contents secpair)
  `(div
    ([id "toc"])
    (b ,($ table-of-contents))
    (ol
     ,@(~>
        secpair reverse
        (slice-by (λ (_ a) (secent-kind a)) _)
        (map
         (match-λ
          [(cons (secent number _ content) subsections)
           `(li
             (a ([id ,(format "section-~a" number)]
                 ,@(render-wikiscribble-elements (mcons 0 null) (mcons 0 null) content)))

             ,@(if (null? content) null
                   `((ol
                      ,@(map (match-λ
                              [(secent number _ content)
                               `(li
                                 (a ([id ,(format "section-~a" number)])
                                    ,@(render-wikiscribble-elements (mcons 0 null) (mcons 0 null) content)))])
                             subsections)))))]
          ['() '()])
         _)))))

(define (add-toc secpair document)
  (append-map (match-λ
               [(and r `(h2 . ,_)) (list (table-of-contents secpair) r)]
               [x (list x)])
              document))


(define (render-output doc)
  (define refpair (mcons 0 null))
  (define secpair (mcons 0 null))
  (~> doc
      (render-wikiscribble-elements refpair secpair _)
      (add-toc (mcdr secpair) _)
      (add-refs (mcdr refpair) _)))

(define-runtime-path environment-path "environments/scribble.rkt")
(define (sandbox-scribble-reader src)
  (list (reader:read-syntax-inside src)))

(define (renderer bytes)
  (define text (bytes->string/utf-8 bytes #\�))
  (with-limits 3 256
    (parameterize ([sandbox-reader sandbox-scribble-reader])
      (define evaluator (make-evaluator environment-path text #:allow-for-require (list environment-path)))
      (call-with-custodian-shutdown
       (λ ()
         (define result (evaluator 'output))
         (call-in-sandbox-context
          evaluator
          (λ () (render-output result))))))))

(register-content-type! "scribble" "Wikiscribble" renderer
                        #:binary #t)
