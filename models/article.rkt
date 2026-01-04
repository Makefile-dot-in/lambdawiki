#lang racket
(require sql
         xml
         threading
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (struct-out article)
 (contract-out
  [get-article-from-path (-> string? (or/c #f article?))]
  [get-article-from-id (-> snowflake? (or/c #f article?))]
  [add-rendering-for-article! (-> snowflake? (listof xexpr?) any)]
  [create-article! (-> string? snowflake? bytes? snowflake?)]
  [edit-article! (-> snowflake? string? snowflake? bytes? any)]
  [delete-article! (-> snowflake? any)]
  [article-full-text-search (-> string?
                                #:limit exact-integer?
                                #:offset exact-integer?
                                (values
                                 (or/c exact-integer? #f)
                                 (listof article?)))]
  [article-all (-> #:limit exact-integer?
                   #:offset exact-integer?
                   (values
                    (or/c exact-integer? #f)
                    (listof article?)))])
 exn:fail:sql:unique-name-violation?)

(struct article (id name content_type source rendering) #:mutable)
(define/match (vector->article _a)
  [((vector id name content_type source rendering))
   (article id name content_type source (and~> rendering sql-null->false
                                               (call-with-input-string read)))])

(define exn:fail:sql:unique-name-violation?
  (curry is-unique-name-violation? "unique_articles_name"))

(define (get-article-from-path name)
  (and~> (query-maybe-row
          (select #:from articles
                  #:where (= name ,name)
                  #:values id name content_type source rendering))

         vector->article))

(define (get-article-from-id id)
  (and~> (query-maybe-row
          (select #:from articles
                  #:where (= id ,id)
                  #:values id name content_type source rendering))

         vector->article))

(define (add-rendering-for-article! id rendering)
  (query-exec (update articles
                      #:set [rendering ,(~s rendering)]
                      #:where (= id ,id))))

(define (create-article! name content-type source)
  (let ([id (new-snowflake)])
    (query-exec (insert #:into articles
                        #:set
                        [id ,id]
                        [name ,name]
                        [content_type ,content-type]
                        [source ,source]))
    id))

(define (edit-article! id name content-type source)
  (query-exec (update articles
                      #:where (= id ,id)
                      #:set
                      [name ,name]
                      [content_type ,content-type]
                      [source ,source]
                      [rendering ,sql-null])))

(define (delete-article! id)
  (query-exec (delete #:from articles #:where (= id ,id))))

(define (article-full-text-search query #:limit limit #:offset offset)
  (define res
    (query-rows
     (select #:from articles
             #:limit ,limit
             #:offset ,offset
             #:where (@@ (to_tsvector name)
                         (to_tsquery ,query))
             #:values
             (ScalarExpr:INJECT ,"count(*) over()")
             id name content_type source rendering)))
  (values (and (pair? res) (~> res car (vector-ref 0)))
          (map (compose vector->article (curryr vector-drop 1)) res)))

(define (article-all #:limit limit #:offset offset)
  (define res
    (query-rows
     (select #:from articles
             #:limit ,limit
             #:offset ,offset
             #:values
             (ScalarExpr:INJECT ,"count(*) over()")
             id name content_type source rendering)))
  (values (and (pair? res) (~> res car (vector-ref 0)))
          (map (compose vector->article (curryr vector-drop 1)) res)))
