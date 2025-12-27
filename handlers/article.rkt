#lang racket
(require forms
         threading
         web-server/dispatch
         web-server/http/xexpr
         web-server/http/redirect
         web-server/http/request-structs
         "../util/db.rkt"
         "../util/permissions.rkt"
         "../renderers/main.rkt"
         "../models/article.rkt"
         "../models/class.rkt"
         "../models/permissions.rkt"
         "../models/content-types.rkt"
         "../views/article.rkt"
         "../i18n/utils.rkt")

(provide article-servlet
         url-to-article
         new-article-url)

(define-values (article-servlet article-url)
  (dispatch-rules
   [("wiki" (string-arg)) view-article]
   [("new-article") #:method (or "get" "post")
                    create-article]))

(register-article-permission! 'general/read "Read articles")
(register-article-permission! 'general/write "Write articles")

(define (view-article _req name)
  (call-with-transaction
   (λ ()
     (define article (get-article-from-path name))
     (check-authorization-for-article 'general/read (article-id article))
     (when (not (article-rendering article))
       (let ([rendering (render-content-type
                         (article-content_type article)
                         (article-source article))])
         (add-rendering-for-article! (article-id article) rendering)
         (set-article-rendering! article rendering)))
     (response/xexpr (article-view article)))))

(struct form-submission (title classes type content))
(define (maybe-binding/file f)
  (binding/file (and f (binding:file? f) f)))

(define (article-body-subform)
  (form* ([type
           (ensure
            binding/number
            (required)
            (one-of
             (for/list ([ct (get-content-types)])
               (cons (content-type-id ct) ct))))]
          
          [source (ensure binding/text (shorter-than 50000))]
          [file maybe-binding/file])
    
    (match/values (values (and~> type content-type-binary) source file)
      [(#f #f _) (err `((source . ,($ mandatory-field))))]
      [(#t _ #f) (err `((file . ,($ mandatory-field))))]
      [(#f s _) (cons type (string->bytes/utf-8 s))]
      [(#t _ f) (cons type f)])))

(define (article-title-and-class-subform)
  (form* ([title (ensure binding/text (shorter-than 50))]
          [classes
            (list-of
             (ensure
              binding/number
              (required)
              (one-of
               (for/list ([cls (get-article-classes)])
                 (cons (article-class-id cls) cls)))))])
    (if (ormap
         (match-λ
          [(struct* article-class ([regex r]))
           (and r (not (regexp-match? r title)))])
         classes)
        (err `((title . ,($ invalid-title-for-class))))
        (ok (cons title classes)))))

(define (create-form)
  (form* ([title-and-class-subform (article-title-and-class-subform)]
          [body-subform (article-body-subform)])
    (form-submission (car title-and-class-subform)
                     (cdr title-and-class-subform)
                     (car body-subform)
                     (cdr body-subform))))

(define (create-article req)
  (match (form-run (create-form) req)
    [`(passed ,(form-submission title classes type content) ,_)
     (check-authorization-for-class 'general/write all-class)
     (for-each (curry check-authorization-for-class 'general/write) classes)
     (create-article! title (content-type-id type) content)
     (redirect-to (url-to-article title) see-other)]
    [(and x (list _ _ render-widget))
     (print x)
     (response/xexpr (create-article-render-form render-widget))]))

(define url-to-article (curry article-url view-article))
(define new-article-url (curry article-url create-article))
