#lang racket
(require xml
         forms
         web-server/http/request-structs
         "base.rkt"
         "../i18n/utils.rkt"
         "../models/class.rkt"
         "../models/article.rkt"
         "../models/content-types.rkt")

(provide
 (contract-out
  [article-view (-> article? xexpr?)]
  [article-create (->* [widget-renderer/c]
                       [(or/c #f (listof string?))]
                       xexpr?)]
  [article-edit (->* [widget-renderer/c]
                     [(or/c #f (listof string?))]
                     xexpr?)]))


(define (article-view article)
  (base-template
   (article-name article)
   `(main
     ([id "article-view"])
     (h1 ,(article-name article))
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

(define (article-edit render-widget [errors #f])
  (base-template
   ($ edit-article)
   (article-edit-render-form
    ($ edit-article)
    ($ article-edit-submit)
    render-widget errors)))
