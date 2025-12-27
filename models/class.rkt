#lang racket
(require sql
         threading
         "../util/db.rkt")

(provide
 (struct-out article-class)
 (contract-out
  [get-article-classes (-> (listof article-class?))]))

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
