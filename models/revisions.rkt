#lang racket
(require sql
         threading
         "../util/db.rkt"
         "../util/snowflake.rkt"
         "../models/user.rkt")

(provide
 (struct-out article-revision)
 (contract-out
  [get-revisions-for-article (-> snowflake?
                                 #:limit exact-integer?
                                 #:offset exact-integer?
                                 (values
                                  (or/c #f exact-integer?)
                                  (listof article-revision?)))]

  [create-revision! (-> snowflake? (or/c string? user?) bytes? any)]))

(struct article-revision (id author) #:transparent)
(define/match (vector->article-revision _v)
  [((vector _ id anon-ip (? sql-null?) (? sql-null?)))
   (article-revision id anon-ip)]
  [((vector _ id (? sql-null?) userid username))
   (article-revision id (user userid username))])

(define (create-revision! article-id author source)
  (query-exec
   (insert #:into revisions
           #:set
           [id ,(new-snowflake)]
           [article ,article-id]
           [userid ,(if (user? author) (user-id author) sql-null)]
           [anon_ip ,(if (string? author) author sql-null)]
           [source ,source])))

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

