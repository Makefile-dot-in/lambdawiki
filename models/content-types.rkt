#lang racket
(require sql
         threading
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (struct-out content-type)
 (contract-out
  [find-content-type (-> string? (or/c #f content-type?))]
  [insert-content-type! (-> string? string? #:binary boolean?
                            snowflake?)]
  [update-content-type! (-> snowflake? string? #:binary boolean?
                            any)]
  [register-content-type! (-> string? string? #:binary boolean?
                              any)]
  [find-content-type-by-id (-> snowflake? content-type?)]
  [get-content-types (-> (listof content-type?))]))

(struct content-type (id textid name binary))

(define/match (vector->content-type _v)
  [((vector id textid name binary))
   (content-type id textid name binary)])

(define (register-content-type! textid human-name #:binary binary)
  (call-with-transaction
   (λ ()
     (match (find-content-type textid)
       [(content-type id _ name binary)
        #:when (not (equal? name human-name))
        (update-content-type! id human-name #:binary binary) id]
       [(content-type id _ _ binary) id]
       [#f (insert-content-type! textid human-name #:binary binary)]))))

(define (find-content-type textid)
  (and~> (query-maybe-row
          (select #:from content_types
                  #:where (= textid ,textid)
                  #:values id textid name is_binary))

         vector->content-type))

(define (find-content-type-by-id id)
  (and~> (query-maybe-row
          (select #:from content_types
                  #:where (= id ,id)
                  #:values id textid name is_binary))

         vector->content-type))

(define (find-id-by-name name)
  (query-maybe-value
   (select #:from articles
           #:where (= name ,name)
           #:values id)))

(define (get-content-types)
  (map vector->content-type
       (query-rows
        (select #:from content_types
                #:values id textid name is_binary))))

(define (insert-content-type! textid name #:binary binary)
  (let ([id (new-snowflake)])
    (query-exec
     (insert #:into content_types
             #:set
             [id ,id]
             [textid ,textid]
             [name ,name]
             [is_binary ,binary]))
    id))

(define (update-content-type! id name #:binary binary)
  (query-exec
   (update content_types
           #:set
           [name ,name]
           [is_binary ,binary]
           #:where [id ,id])))
