#lang racket
(require sql
         xml
         threading
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (struct-out article)
 (contract-out
  [get-article-from-path (-> string? article?)]
  [add-rendering-for-article! (-> snowflake? (listof xexpr?) any)]
  [create-article! (-> string? snowflake? bytes? snowflake?)]
  [edit-article! (-> snowflake? string? snowflake? bytes? any)])
 exn:fail:sql:unique-name-violation?)

(struct article (id name content_type source rendering) #:mutable)
(define/match (vector->article _a)
  [((vector id name content_type source rendering))
   (article id name content_type source (and~> rendering sql-null->false
                                               (call-with-input-string read)))])

(define/match (exn:fail:sql:unique-name-violation? _n)
  [((struct* exn:fail:sql ([sqlstate (== unique-constraint-violation)]
                           [info (app (curry assoc 'constraint) "unique_articles_name")])))
   #t]
  [(_) #f])

(define (get-article-from-path name)
  (and~> (query-maybe-row
          (select #:from articles
                  #:where (= name ,name)
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
                        [id ,(new-snowflake)]
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
