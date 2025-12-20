#lang racket
(require "session.rkt")


(struct exn:fail:unauthorized exn:)

(define (assert-authorized . args)
  (when (not (current-user))
    (raise (exn:fail:unauthenticated)))
  
  (when (not (null? args))
    (error "unimplemented")))
