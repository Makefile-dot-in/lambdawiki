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
  [create-article! (-> string? snowflake? bytes? any)]))


(struct article (id name content_type source rendering) #:mutable)
(define/match (vector->article _a)
  [((vector id name content_type source rendering))
   (article id name content_type source (and~> rendering sql-null->false
                                               (call-with-input-string read)))])

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
  (query-exec (insert #:into articles
                      #:set
                      [id ,(new-snowflake)]
                      [name ,name]
                      [content_type ,content-type]
                      [source ,source])))
