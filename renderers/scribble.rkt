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

(struct secent (number kind content) #:transparent)

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
                    [alignment (in-sequences alignments (in-cycle '(left)))])
           `(,elem ([class ,(case alignment
                              ['left "align-left"]
                              ['right "align-right"])])
                   ,@(cond
                      [(list? cell)
                       (render-wikiscribble-elements refs secs cell)]
                      [else
                       (list (render-wikiscribble-element refs secs cell))])))))

(define (render-wikiscribble-element refs secs f)
  (define recurse (curry render-wikiscribble-elements refs secs))
  (match f
    [`(bold   . ,r)
     `(b . ,(recurse r))]
    [`(italic . ,r)
     `(i . ,(recurse r))]
    [`(section . ,r)
     (define n (append-section secs r))
     `(h2 ([id ,(format "section-~a" n)]) . ,(recurse r))]
    [`(subsubsection . ,r)
     (define n (append-subsection refs r))
     `(h3 ([id ,(format "section-~a" n)]) . ,(recurse r))]
    [`(ref . ,r)
     (define n (append-refs refs r))
     `(a ([href ,(format "#ref-~a" n)] [class "ref"])
         ,(format "[~a]" n))]
    [`(table ,alignments ,headers ,rows)
     `(table
       ,@(if (null? headers) null
             `((thead ,(generate-row refs secs 'th alignments headers))))

       (tbody
        ,@(for/list ([row rows])
            (generate-row refs secs 'td alignments row))))]
    [`(link ,target . ,appearance)
     `(a ([href ,(url-to-article target)]) ,@(recurse appearance))]
    [(? string?) f]))

(define (render-wikiscribble-elements refs secs lst)
  (map (curry render-wikiscribble-element refs secs) lst))

(define (add-refs refs secpair forms)
  (if (null? refs) forms
      (begin
        (append-section secpair (list ($ reference-title)))
        (append
         forms
         `((h2 ,($ reference-title))
           (ol
            ,@(for/list ([(ref i) (in-indexed (reverse refs))])
                `(li
                  (a ([id ,(format "ref-~a" (add1 i))])
                     (cite ,@(map (curry render-wikiscribble-element
                                         (mcons 0 null)
                                         (mcons 0 null))
                                  ref)))))))))))


(define (table-of-contents secs)
  `(div
    ([id "toc"])
    (h3 ,($ table-of-contents))
    (ol
     (li (a ([href "#article-title"]) ,($ article-introduction)))
     ,@(~>
        secs reverse
        (slice-by (λ (a b)
                    (match* ((secent-kind a) (secent-kind b))
                      [('subsection 'section) #t]
                      [(_ _) #f])) _)
        (map
         (match-λ
          [(cons (secent number _ content) subsections)
           `(li
             (a ([href ,(format "#section-~a" number)])
                ,@(render-wikiscribble-elements
                   (mcons 0 null) (mcons 0 null) content))

             ,@(if (null? content) null
                   `((ol
                      ,@(map
                         (match-λ
                          [(secent number _ content)
                           `(li
                             (a ([href ,(format "#section-~a" number)])
                                ,@(render-wikiscribble-elements
                                   (mcons 0 null) (mcons 0 null)
                                   content)))])
                         subsections)))))]
          ['() '()])
         _)))))

(define (add-toc secpair document)
  (match document
    [`((h2 . ,_) . ,_) (cons (table-of-contents secpair) document)]
    [(cons el els) (cons el (add-toc secpair els))]))

(define (render-output doc)
  (define refpair (mcons 0 null))
  (define secpair (mcons 0 null))
  (~> doc
      (render-wikiscribble-elements refpair secpair _)
      ((λ (e) (add-refs (mcdr refpair) secpair e)))
      ((λ (e) (add-toc (mcdr secpair) e)))))

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
          (λ ()
            (render-output result))))))))

(register-content-type! "scribble" "Wikiscribble" renderer
                        #:binary #t)
