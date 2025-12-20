#!/usr/bin/env racket
#lang racket
(require "init.rkt")

(define stop (start))
(with-handlers ([exn:break? (λ (e) (stop))])
  (sync/enable-break never-evt))
