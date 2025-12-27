#lang racket
(require xml
         threading
         (prefix-in model: "../models/content-types.rkt")
         "../util/snowflake.rkt")

(provide
 (contract-out
  [register-content-type! (-> string? string? (-> bytes? (listof xexpr?))
                              #:binary boolean?
                              any)]
  [render-content-type (-> snowflake? bytes? (listof xexpr?))]))

(define textid-registry (make-hash))
(define id-registry (make-hash))

(struct registry-entry (id textid human-name renderer binary))

(define (register-content-type! textid human-name renderer
                                #:binary binary)
  (define id (model:register-content-type! textid human-name
                                           #:binary binary))
  (define entry (registry-entry id textid human-name renderer binary))
  (hash-set! textid-registry textid entry)
  (hash-set! id-registry id entry)
  id)

(define (render-content-type id source)
  (~> id-registry (hash-ref id) registry-entry-renderer (_ source)))
