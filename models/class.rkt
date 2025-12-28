#lang racket
(require sql
         threading
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (struct-out article-class)
 (contract-out
  [get-article-classes (-> (listof article-class?))]
  [get-classes-of-article (-> snowflake? (listof snowflake?))]
  [set-article-classes! (-> snowflake? (listof snowflake?) any)]))

(struct article-class (id name special regex ordering))

(define/match (vector->article-class _c)
  [((vector id name special regex ordering))
   (article-class id name special (and~> regex sql-null->false regexp) ordering)])

(define (get-article-classes)
  (map
   vector->article-class
   (query-rows
    (select #:from classes
            #:order-by ordering
            #:where (not special)
            #:values id name special regex ordering))))

(define (get-classes-of-article article-id)
  (query-list
   (select #:from
           (inner-join
            article_classes classes
            #:on (= article_classes.class classes.id))
           #:order-by ordering
           #:where (and (not special)
                        (= article_classes.article ,article-id))
           #:values id)))

(define (set-article-classes! article-id class-ids)
  (query-exec
   (delete #:from article_classes
           #:where
           (and (= article ,article-id)
                (<> class #:some ,class-ids))))

  (when (not (null? class-ids))
    (query-exec
     (insert #:into article_classes
             #:columns article class
             #:from
             (TableExpr:AST
              ,(make-values*-table-expr-ast
                (for/list ([class-id class-ids])
                  (map value->scalar-expr-ast
                       (list article-id class-id)))))))))
