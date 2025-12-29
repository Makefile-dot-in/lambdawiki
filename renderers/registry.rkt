#lang racket
(require xml
         threading
         (prefix-in model: "../models/content-types.rkt")
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (contract-out
  [register-content-type! (->* [string?
                                string?
                                (-> snowflake? bytes? (listof xexpr?))
                                #:binary boolean?]
                               [#:mime-type bytes?]
                              any)]
  [render-content-type (-> snowflake? snowflake? bytes? (listof xexpr?))]
  [content-type-mime (-> snowflake? bytes?)]))

(define textid-registry (make-hash))
(define id-registry (make-hash))

(struct registry-entry (id textid human-name renderer binary mime))

(define (register-content-type!
         textid human-name renderer
         #:binary binary
         #:mime-type [mime
                      (if binary
                          #"application/octet-stream"
                          #"text/plain; charset=utf-8")])
  (when (current-connection)
    (define id (model:register-content-type! textid human-name
                                             #:binary binary))
    (define entry (registry-entry id textid human-name
                                  renderer binary mime))
    (hash-set! textid-registry textid entry)
    (hash-set! id-registry id entry)
    id))

(define (content-type-mime id)
  (or (and~> id-registry (hash-ref id) registry-entry-mime)
      #"application/octet-stream"))

(define (render-content-type id article-id source)
  (~> id-registry (hash-ref id) registry-entry-renderer (_ article-id source)))
