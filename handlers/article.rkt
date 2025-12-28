#lang racket
(require forms
         threading
         (prefix-in srfi1: srfi/1)
         web-server/dispatch
         web-server/http/xexpr
         web-server/http/redirect
         web-server/http/request-structs
         "../config.rkt"
         "../util/db.rkt"
         "../util/misc.rkt"
         "../util/permissions.rkt"
         "../renderers/main.rkt"
         "../models/article.rkt"
         "../models/class.rkt"
         "../models/permissions.rkt"
         "../models/revisions.rkt"
         "../models/content-types.rkt"
         "../views/article.rkt"
         "../i18n/utils.rkt")

(provide article-servlet
         url-to-article
         new-article-url
         edit-article-url
         url-to-article-revisions)

(define-values (article-servlet article-url)
  (dispatch-rules
   [("") (λ (req) (redirect-to (url-to-article (main-page)) see-other))]
   [("wiki" (string-arg)) view-article]
   [("wiki" (string-arg) "edit") #:method (or "get" "post")
                                 edit-article]
   [("wiki" (string-arg) "revisions") article-revisions]
   [("new-article") #:method (or "get" "post")
                    create-article]))

(register-article-permission! 'general/read "Read articles")
(register-article-permission! 'general/create "Create articles")
(register-article-permission! 'general/write "Write articles")
(register-article-permission! 'class/add "Add class")
(register-article-permission! 'class/remove "Remove class")

(define (view-article _req name)
  (call-with-transaction
   (λ ()
     (define article (get-article-from-path name))
     (when (not article) (not-found name))
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


(define (article-edit-form req renderer on-submit
                           #:defaults [defaults (hash)])
  (match (form-run (create-form) req #:defaults defaults)
    [`(passed ,(form-submission title classes type content) ,render-widget)
     (with-handlers
       ([exn:fail:sql:unique-name-violation?
         (λ (e)
           (response/xexpr
            (renderer render-widget (list ($ title-must-be-unique)))))])
       (on-submit title classes (content-type-id type) content)
       (redirect-to (url-to-article title) see-other))]
    [(list _ _ render-widget)
     (response/xexpr (renderer render-widget))]))


(define (create-article req)
  (article-edit-form
   req article-create
   (λ (title classes ctid content)
     (call-with-transaction
      (λ ()
        (check-authorization-for-class 'general/create all-class)
        (for-each (curry check-authorization-for-class 'class/add)
                  classes)
        (~> (create-article! title ctid content)
            (set-article-classes! classes)))))))

(define (edit-article req name)
  (call-with-transaction
   (λ ()
     (match-define (article id old-title old-ctid old-content _)
       (get-article-from-path name))
     (define old-ct (find-content-type-by-id old-ctid))
     (define old-classes (get-classes-of-article id))
     (article-edit-form
      req (curry article-edit old-title)
      #:defaults
      (hash
       "title-and-class-subform.title" old-title
       "title-and-class-subform.classes" old-classes
       "body-subform.type" (number->string old-ctid)
       "body-subform.source" (if (content-type-binary old-ct) ""
                                 old-content))

      (λ (title classes ctid content)
        (check-authorization-for-article 'general/write id)

        ; we check which articles have been added and removed and
        ; check their permissions
        (for-each (curry check-authorization-for-class 'class/remove)
                  (srfi1:lset-difference equal? old-classes classes))

        (for-each (curry check-authorization-for-class 'class/add)
                  (srfi1:lset-difference equal? classes old-classes))

        (edit-article! id title ctid content)
        (set-article-classes! id classes))))))

(define (article-revisions req name)
  (define articleval (get-article-from-path name))
  (when (not articleval) (not-found name))
  (define id (article-id articleval))

  (define offset (or (request-query-param req 'offset) 0))
  (define limit (or (request-query-param req 'limit) 50))

  (define-values (num-revisions revisions)
    (get-revisions-for-article id #:limit limit #:offset offset))

  (response/xexpr (article-revisions-view
                   name num-revisions limit offset revisions)))

(define url-to-article (curry article-url view-article))
(define new-article-url (curry article-url create-article))
(define edit-article-url (curry article-url edit-article))
(define url-to-article-revisions
  (curry article-url article-revisions))
