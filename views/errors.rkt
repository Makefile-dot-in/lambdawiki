#lang racket
(require "base.rkt")

(define (error-template short long)
  (base-template
   short
   `(main
     (div ,short)
     ,long)))
   
