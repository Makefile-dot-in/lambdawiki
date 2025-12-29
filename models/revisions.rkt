#lang racket
(require sql
         xml
         threading
         "../util/db.rkt"
         "../util/snowflake.rkt"
         "../models/user.rkt")

(provide
 (struct-out article-revision)
 (struct-out full-revision)
 (contract-out
  [get-revisions-for-article (-> snowflake?
                                 #:limit exact-integer?
                                 #:offset exact-integer?
                                 (values
                                  (or/c #f exact-integer?)
                                  (listof article-revision?)))]

  [create-revision! (-> snowflake? (or/c string? user?) bytes? any)]
  [get-full-article-revision (-> snowflake? (or/c #f full-revision?))]
  [add-revision-rendering! (-> snowflake? (listof xexpr?) any)]))

(struct article-revision (id author) #:transparent)
(define/match (vector->article-revision _v)
  [((vector _ id anon-ip (? sql-null?) (? sql-null?)))
   (article-revision id anon-ip)]
  [((vector _ id (? sql-null?) userid username))
   (article-revision id (user userid username))])

(struct full-revision article-revision (article-id
                                        content-type-id
                                        source rendering)
  #:transparent
  #:mutable)

(define/match (vector->full-revision _v)
  [((vector id article-id ctid source rendering
            anon-ip (? sql-null?) (? sql-null?)))
   (full-revision id article-id anon-ip article-id ctid source
                  (and~> rendering
                         sql-null->false
                         (call-with-input-string read)))]
  [((vector id article-id ctid source rendering
            (? sql-null?) userid username))
   (full-revision id (user userid username) article-id ctid source
                  (and~> rendering
                         sql-null->false
                         (call-with-input-string read)))])

(define (create-revision! article-id author source)
  (query-exec
   (insert #:into revisions
           #:set
           [id ,(new-snowflake)]
           [article ,article-id]
           [userid ,(if (user? author) (user-id author) sql-null)]
           [anon_ip ,(if (string? author) author sql-null)]
           [source ,source])))

(define (get-full-article-revision id)
  (and~> (query-maybe-row
          (select
           #:from (left-join
                   (left-join
                    revisions articles
                    #:on (= revisions.article articles.id))
                   users
                   #:on (= users.id revisions.userid))
           #:where (= revisions.id ,id)
           #:values
           revisions.id
           revisions.article
           articles.content_type
           revisions.source
           revisions.rendering
           revisions.anon_ip
           users.id
           users.username))
         vector->full-revision))

(define (add-revision-rendering! id rendering)
  (query-exec (update revisions
                      #:where (= id ,id)
                      #:set [rendering ,(~s rendering)])))

(define (get-revisions-for-article id #:limit limit #:offset offset)
  (define res
    (query-rows
     (select #:from (left-join
                     revisions users
                     #:on (= users.id revisions.userid))
             #:where (= article ,id)
             #:order-by revisions.id #:desc
             #:limit ,limit
             #:offset ,offset
             #:values
             (ScalarExpr:INJECT ,"count(*) over()")
             revisions.id
             revisions.anon_ip
             users.id
             users.username)))

  (values (if (null? res) #f (~> res car (vector-ref 0)))
          (map vector->article-revision res)))

