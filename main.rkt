#!/usr/bin/env racket
#lang racket
(require threading
         net/url
         (prefix-in files:     web-server/dispatchers/dispatch-files)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter:    web-server/dispatchers/dispatch-filter)
         (prefix-in logresp:   web-server/dispatchers/dispatch-logresp)
         
         web-server/dispatchers/filesystem-map
         web-server/servlet-env
         web-server/web-server

         "config.rkt")

(define files
  (~> (make-url->path "static")
      (compose (λ (u) (struct-copy url u [path (cdr (url-path u))])))
      (files:make #:url->path _)
      (filter:make #rx"^/static/" _)))

(define dispatcher
  (~> (sequencer:make files)
      (logresp:make #:log-path (current-output-port)
                    #:format 'apache-default
                    _)))
      
(define config-file (make-parameter "config.rktd"))

(define (with-config-from-command-line f)
  (parameterize ()
    (command-line
     #:program "lambdawiki"
     #:once-each
     [("-c" "--config")
      fn
      "Path to a config file containing S-expressions"
      (config-file fn)]
     #:args ()
     (call-with-input-file (config-file)
       (λ (config-port)
         (with-config config-port f))))))

(define stop
  (with-config-from-command-line
    (λ () (serve #:dispatch dispatcher
                 #:port 8080))))

(with-handlers ([exn:break? (λ (e) (stop))])
  (sync/enable-break never-evt))

