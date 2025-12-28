#lang racket
(require xml
         forms
         threading

         net/url

         racket/date
         racket/lazy-require

         web-server/http/request-structs

         "base.rkt"
         "../i18n/utils.rkt"
         "../util/misc.rkt"
         "../util/snowflake.rkt"
         "../models/class.rkt"
         "../models/user.rkt"
         "../models/article.rkt"
         "../models/revisions.rkt"
         "../models/content-types.rkt")

(provide
 (contract-out
  [article-view (-> article? xexpr?)]
  [article-create (->* [widget-renderer/c]
                       [(or/c #f (listof string?))]
                       xexpr?)]
  [article-edit (->* [string? widget-renderer/c]
                     [(or/c #f (listof string?))]
                     xexpr?)]
  [article-revisions-view (-> string? (or/c #f exact-integer?)
                              exact-integer? exact-integer?
                              (listof article-revision?)
                              xexpr?)]))

(lazy-require ["../handlers/article.rkt"
               (url-to-article edit-article-url url-to-article-revisions)])

(define (article-base-template title article-title body)
  (base-template
   title
   `(div
     ([id "article-base"])
     (nav
      (ul
       ,@(for/list ([link `((l article-links-page
                             ,(url-to-article article-title))
                            (1r article-links-edit
                             ,(edit-article-url article-title))
                            (r article-links-revisions
                               ,(url-to-article-revisions article-title)))])
           (match-define (list align key url) link)
           (define alignment-class (match align
                                     ['l "align-l"]
                                     ['r "align-r"]
                                     ['1r "align-1r"]))
           (if (equal? (~> (current-url) string->url url-path)
                       (~> url string->url url-path))
               `(li ([class ,alignment-class])
                    ,($ ,key))
               `(li ([class ,alignment-class])
                    (a ([href ,url]) ,($ ,key)))))))

     ,body)))

(define (article-view article)
  (article-base-template
   (article-name article)
   (article-name article)
   `(main
     ([id "article-view"])
     (h1 ([id "article-title"])
         ,(article-name article))
     (article
      ,@(article-rendering article)))))

(define (article-edit-render-form title submitlbl render-widget
                                  [errors #f])
  (define render-title-and-class-subwidget
    (widget-namespace "title-and-class-subform" render-widget))
  (define render-body-subwidget
    (widget-namespace "body-subform" render-widget))

  `(main
    ([id "article-page"])
    (h1 ,title)
    (form
     ([method "post"])
     ,@(if (not errors) null
           `((ul ([class "error"])
                 ,@(for/list ([e errors]) `(li ,e)))))
     
     (label ,($ article-title)
            ,(render-title-and-class-subwidget "title" (widget-text)))
     ,@(render-title-and-class-subwidget "title" (widget-errors))

     (label ,($ article-content-type)
            ,(render-body-subwidget
              "type"
              (λ (name binding _errors)
                `(select
                  ([name ,name])
                  ,@(for/list ([ct (get-content-types)])
                      `(option
                        ([value ,(number->string (content-type-id ct))]
                         ,@(if (and binding (equal? (binding:form-value binding)
                                                    (content-type-id ct)))
                               '((selected "selected"))
                               null)
                         ,@(if (not (content-type-binary ct)) null
                               '((data-binary ""))))
                        ,(content-type-name ct)))))))
     ,@(render-body-subwidget "type" (widget-errors))

     (label ,($ article-class)
            (select ([id "class"])
                    ,@(for/list ([class (get-article-classes)])
                        `(option ([value
                                   ,(number->string
                                     (article-class-id class))])
                                 ,(article-class-name class)))))

     (div ([class "article-content-section"])
          (label ,($ article-content)
                 ,(render-body-subwidget
                   "source"
                   (widget-textarea
                    #:attributes `([rows "5"]))))
          ,@(render-body-subwidget "source" (widget-errors)))

     (div ([class "article-content-upload"])
          (label ,($ article-upload)
                 ,(render-body-subwidget "file" (widget-file)))
          ,@(render-body-subwidget "file" (widget-errors)))

     (input ([type "submit"]
             [value ,submitlbl])))))

(define (article-create render-widget [errors #f])
  (base-template
   ($ create-article)
   (article-edit-render-form
    ($ create-article)
    ($ article-create-submit)
    render-widget errors)))

(define (article-edit article-title render-widget [errors #f])
  (article-base-template
   ($ edit-article)
   article-title
   (article-edit-render-form
    ($ edit-article)
    ($ article-edit-submit)
    render-widget errors)))

(define (check-xexpr x) (println (xexpr? x)) x)

(define (article-revisions-view title count limit offset revisions)
  (article-base-template
   ($ article-revisions ,title)
   title
   `(main
     ([id "article-revisions"])

     (h1 ,($ article-revisions ,title))
     ,@(if (not count) null
           `(p ,($ found-revisions ,count)))
     ,(generate-table
       (list ($ article-revisions-time)
             ($ article-revisions-author)
             ($ article-revisions-actions))

       (for/list ([r revisions])
         (list (~> r article-revision-id snowflake->date
                   (date->string #t))
               (match (article-revision-author r)
                 [(struct user (_ username)) username]
                 [ip ip])
               "[placeholder]")))

     (form
      ([method "get"])
      (label
       ,(format "~a:" ($ article-revisions-limit-label))
       (select
        ([name "limit"])
        ,@(for/list ([l '("25" "50" "100")])
            (if (equal? l limit)
                `(option ([value ,l] [selected ""]) ,l)
                `(option ([value ,l]) ,l)))))

      (input ([type "submit"]
              [value ,($ article-revisions-limit-submit)])))
     
     ,@(if (equal? offset 0) null
           `((a ([href
                  ,(url-with-params
                    ""
                    `((offset . ,(max 0 (- limit offset)))
                      (limit . ,limit)))])
                ,(format "[~a]" ($ article-revisions-next)))))

     ,@(if (offset . < . (+ limit offset)) null
           `((a ([href
                  ,(url-with-params
                    ""
                    `((offset . ,(max 0 (+ limit offset)))
                      (limit . ,limit)))])
                ,(format "[~a]" ($ article-revisions-prev))))))))
