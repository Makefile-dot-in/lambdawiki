#lang racket
(require forms
         threading
         (prefix-in srfi1: srfi/1)
         net/url
         net/uri-codec
         web-server/dispatch
         web-server/http/xexpr
         web-server/http/redirect
         web-server/http/request-structs
         web-server/http/response-structs
         "../config.rkt"
         "../util/db.rkt"
         "../util/misc.rkt"
         "../util/session.rkt"
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
         url-to-article-raw
         new-article-url
         edit-article-url
         delete-article-url
         url-to-article-revisions
         url-to-article-revision
         wiki-by-id)

;; defines all the paths used by articles
(define-values (article-servlet article-url)
  (dispatch-rules
   [("") (λ (_) (redirect-to (url-to-article (main-page)) see-other))]
   [("wiki-by-id" (number-arg) (string-arg) ...)
    article-id-redirect]
   [("wiki" (string-arg)) view-article]
   [("wiki" (string-arg) "raw") view-article-raw]
   [("wiki" (string-arg) "edit") #:method (or "get" "post")
                                 edit-article]
   [("wiki" (string-arg) "delete") #:method "post" delete-article]
   [("wiki" (string-arg) "revisions") article-revisions]
   [("wiki" (string-arg) "revision" (number-arg)) article-revision]
   [("new-article") #:method (or "get" "post")
                    create-article]
   [("all-articles") all-articles]
   [("search") article-search]))

(register-article-permission! 'general/read "Read articles")
(register-article-permission! 'general/create "Create articles")
(register-article-permission! 'general/write "Write articles")
(register-article-permission! 'general/move "Move articles")
(register-article-permission! 'general/delete "Delete articles")
(register-article-permission! 'revision/list "List revisions")
(register-article-permission! 'revision/read "Read a revision")
(register-article-permission! 'class/add "Add class")
(register-article-permission! 'class/remove "Remove class")

;; used for forming permalinks to articles
(define (article-id-redirect _req id path)
  (define u (string->url "/wiki"))
  (define article (get-article-from-id id))
  (redirect-to
   (format "/wiki/~a/~a"
           (uri-encode (article-name article))
           (string-join (map uri-encode path) "/"))
   see-other))

;; renders and returns an article
(define (view-article _req name)
  (call-with-transaction
   (λ ()
     (define article (get-article-from-path name))
     (when (not article) (not-found name))
     (check-authorization-for-article 'general/read (article-id article))
     ;; check if there's a cached article rendering, if not, then
     ;; render and cache
     (when (not (article-rendering article))
       (let ([rendering (render-content-type
                         (article-content_type article)
                         (article-id article)
                         (article-source article))])
         (add-rendering-for-article! (article-id article) rendering)
         (set-article-rendering! article rendering)))
     (response/xexpr (article-view article)))))

;; returns an article without rendering, after fetching the MIME type
(define (view-article-raw _req name)
  (define article (get-article-from-path name))
  (when (not article) (not-found name))
  (check-authorization-for-article 'general/read (article-id article))
  (response/output
   #:mime-type (content-type-mime (article-content_type article))
   (λ (out)
     (write-bytes (article-source article) out))))

;; delete an article
(define (delete-article _req name)
  (define articleval (get-article-from-path name))
  (when (not articleval) (not-found name))
  (check-authorization-for-article 'general/delete (article-id articleval))
  (call-with-transaction (λ () (delete-article! (article-id articleval))))
  (redirect-to "/" see-other))

;; represents a submission of an article edit or create form
(struct edit-form-submission (title classes type content))

;; (maybe-binding/file f) is #f if f or (binding:file? f) is #f, else it returns f
(define (maybe-binding/file f)
  (binding/file (and f (binding:file? f) f)))

;; this function returns the subform responsible for the body of the
;; edit form, which validates whether the right type of content was
;; passed in
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
      [(#t _ f) (cons type (binding:file-content f))])))

;; this function returns the subform responsible for the title and
;; class, which validates whether the title matches the regex
;; prescribed by the title
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

;; creates a create or edit form
(define (create-edit-form)
  (form* ([title-and-class-subform (article-title-and-class-subform)]
          [body-subform (article-body-subform)])
    (edit-form-submission (car title-and-class-subform)
                          (cdr title-and-class-subform)
                          (car body-subform)
                          (cdr body-subform))))


(define (article-list-combine _k v1 v2)
  (if (pair? v1)
      (append v1 (list v2))
      (list v1 v2)))

;; runs a create an edit form, passing in req for the request,
;; renderer for the form renderer, and on-submit for the
;; post-submission action, using defaults as the form defaults.
(define (article-edit-form req renderer on-submit
                           #:defaults [defaults (hash)])
  (match (form-run
          (create-edit-form) req
          #:defaults defaults
          #:combine article-list-combine)
    [`(passed ,(edit-form-submission title classes type content) ,render-widget)
     (with-handlers
       ([exn:fail:sql:unique-name-violation?
         (λ (e)
           (response/xexpr
            (renderer render-widget (list ($ title-must-be-unique)))))])
       (on-submit title classes (content-type-id type) content)
       (redirect-to (url-to-article title) see-other))]
    [(list _ _ render-widget)
     (response/xexpr (renderer render-widget))]))

;; returns the author of the request for the purposes of adding a
;; revision
(define (current-author req)
  (or (current-user) (request-client-ip req)))

;; handles showing the article creation form and handling its
;; submission
(define (create-article req)
  (article-edit-form
   req article-create
   (λ (title classes ctid content)
     (call-with-transaction
      (λ ()
        ;; check authorization
        (check-authorization-for-class 'general/create all-class)
        (for-each (curry check-authorization-for-class 'class/add)
                  classes)

        ;; create the article
        (define id (create-article! title ctid content))
        (set-article-classes! id classes)
        (create-revision! id (current-author req) content))))))

;; handles showing the article edit form and handling its submission
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

        ;; we check which classes have been added and removed and
        ;; check their permissions
        (for-each (curry check-authorization-for-class 'class/remove)
                  (srfi1:lset-difference equal? old-classes classes))

        (for-each (curry check-authorization-for-class 'class/add)
                  (srfi1:lset-difference equal? classes old-classes))

        (when (not (equal? title old-title))
          (check-authorization-for-article 'general/move id))

        ;; submit the edit
        (edit-article! id title ctid content)
        (set-article-classes! id classes)
        (create-revision! id (current-author req) content))))))

;; returns the list of article revisions
(define (article-revisions req name)
  ;; retrieves the article for its ID
  (define articleval (get-article-from-path name))
  (when (not articleval) (not-found name))
  (define id (article-id articleval))
  (check-authorization-for-article 'revision/list id)

  ;; retrieves query parameters
  (define offset (or (and~> (request-query-param req 'offset)
                            string->number) 0))
  (define limit (or (and~> (request-query-param req 'limit)
                           string->number (min 100)) 50))

  ;; retrieves revisions from database, with num-revisions
  ;; being the total number of revisions
  (define-values (num-revisions revisions)
    (get-revisions-for-article id #:limit limit #:offset offset))

  (response/xexpr (article-revisions-view
                   name num-revisions limit offset revisions)))

;; searches among article titles
(define (article-search req)
  ;; retrieves query parameters
  (define query (or (request-query-param req 'q) ""))
  (define offset (or (and~> (request-query-param req 'offset) string->number) 0))
  (define limit (or (and~> (request-query-param req 'limit)
                           string->number (min 100))
                    25))

  ;; retrieves the query from the database
  (define-values (num-results results)
    (article-full-text-search query #:limit limit #:offset offset))

  (response/xexpr
   (article-search-view
    query results
    #:count num-results
    #:limit limit
    #:offset offset)))

;; renders a particular article revision
(define (article-revision _req name id)
  (define revision (get-full-article-revision id))
  (when (not revision) (revision-not-found id))

  (check-authorization-for-article 'revision/read id)
  ;; check if the revision's rendering is cache, if not, then render
  ;; it and save it to the cache
  (when (not (full-revision-rendering revision))
    (let ([rendering (render-content-type
                      (full-revision-content-type-id revision)
                      (full-revision-article-id revision)
                      (full-revision-source revision))])
      (add-revision-rendering! id rendering)
      (set-full-revision-rendering! revision rendering)))
  (response/xexpr (article-revision-view name revision)))

;; retrieves a paginated list of all articles
(define (all-articles req)
  (define offset (or (and~> (request-query-param req 'offset) string->number) 0))
  (define limit (or (and~> (request-query-param req 'limit)
                           string->number (min 100))
                    25))
  (define-values (num-articles articles)
    (article-all #:limit limit #:offset offset))
  (response/xexpr
   (article-all-view
    articles
    #:count num-articles
    #:offset offset
    #:limit limit)))

;; partially applied versions of article-url so that it can be lazy
;; loaded in the sense of racket/lazy-require without causing
;; dependency cycles
(define url-to-article (curry article-url view-article))
(define url-to-article-raw (curry article-url view-article-raw))
(define new-article-url (curry article-url create-article))
(define edit-article-url (curry article-url edit-article))
(define delete-article-url (curry article-url delete-article))
(define url-to-article-revisions
  (curry article-url article-revisions))
(define url-to-article-revision
  (curry article-url article-revision))
(define (wiki-by-id id . segments)
  (article-url article-id-redirect id segments))
