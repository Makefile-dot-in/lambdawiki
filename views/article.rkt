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
         "../models/content-types.rkt"
         "../renderers/main.rkt")

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
                              xexpr?)]
  [article-revision-view (-> string? full-revision? xexpr?)]
  [article-search-view (-> string? (listof article?)
                           #:count (or/c #f exact-integer?)
                           #:offset exact-integer?
                           #:limit exact-integer?
                           xexpr?)]
  [article-all-view (-> (listof article?)
                        #:count (or/c #f exact-integer?)
                        #:offset exact-integer?
                        #:limit exact-integer?
                        xexpr?)]))

(lazy-require ["../handlers/article.rkt"
               (url-to-article
                edit-article-url
                url-to-article-revisions
                url-to-article-revision)])

(define/contract (article-base-template title article-title body)
  (-> string? string? xexpr? xexpr?)
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
     ([method "post"]
      [enctype "multipart/form-data"])
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

     (label
      ,($ article-class)
      ,@(render-title-and-class-subwidget
         "classes"
         (λ (_name bindings _errors)
           (cond
             [(pair? bindings)
              `(div ([id "class-list"])
                    (for/list ([binding bindings])
                      `(input
                        ([type "text"]
                         [class "class-input"]
                         [value ,binding]
                         [disabled "disabled"]))))]
             [else null])))

      (select ([id "class-select"])
        ,@(for/list ([class (get-article-classes)])
            `(option
              ([value ,(number->string (article-class-id class))])
              ,(article-class-name class))))

      (button
       ([id "add-class"]
        [onclick "addClass()"]
        [type "button"])
       ,($ class-add-button)))

     ,@(render-title-and-class-subwidget "classes" (widget-errors))

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
             [value ,submitlbl]))

     (script ([src "/static/article-form.js"])))))

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

(define (article-revisions-view title count limit offset revisions)
  (article-base-template
   ($ article-revisions ,title)
   title
   `(main
     ([id "article-revisions"])

     (h1 ,($ article-revisions ,title))
     ,@(if (not count) null
           `((p ,($ found-revisions ,count))))
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
               `(div
                 (a ([href ,(url-to-article-revision
                                  title (article-revision-id r))])
                    ,(format
                      "[~a]" ($ article-revisions-action-view)))))))

     ,@(pagination-controls
        #:limit limit
        #:offset offset
        #:count count
        #:per-page-label ($ article-revisions-limit-label)))))

(define (article-revision-view title revision)
  (define page-title
    ($ revision-title ,(article-revision-id revision)
       ,(match (article-revision-author revision)
          [(user _ username) username]
          [(and ip (? string?)) ip])))
  (article-base-template
   page-title title
   `(main
     ([id "article-view"])
     (h1 ([id "article-title"]) ,page-title)
     (article ,@(full-revision-rendering revision)))))

(define (article-paginated-list
         results
         #:count count
         #:offset offset
         #:limit limit
         #:title title)
  `(main
   ([id "article-list"])
   (h1 ,title)
   (p ,($ found-articles ,count))
   ,(generate-table
     (list ($ search-article-name)
           ($ search-article-type))
     (for/list ([r results])
       (list
        `(a ([href ,(url-to-article (article-name r))])
            ,(article-name r))
        (content-type-human-name
         (article-content_type r)))))
   ,@(pagination-controls
      #:limit limit
      #:offset offset
      #:count count
      #:per-page-label ($ article-search-limit-label))))

(define (article-search-view query results #:count count #:offset offset #:limit limit)
  (base-template
   ($ search-results ,query)
   #:search-query query
   (article-paginated-list
    results
    #:title ($ search-results ,query)
    #:count count
    #:offset offset
    #:limit limit)))

(define (article-all-view results #:count count #:offset offset #:limit limit)
  (base-template
   ($ all-articles)
   (article-paginated-list
    results
    #:title ($ all-articles)
    #:count count
    #:offset offset
    #:limit limit)))
