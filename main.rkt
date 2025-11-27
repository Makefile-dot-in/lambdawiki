#lang racket
(require threading
         net/url
         (prefix-in files:     web-server/dispatchers/dispatch-files)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter:    web-server/dispatchers/dispatch-filter)
         (prefix-in logresp:   web-server/dispatchers/dispatch-logresp)
         
         web-server/dispatchers/filesystem-map
         web-server/servlet-env
         web-server/web-server)

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

(define stop
  (serve #:dispatch dispatcher
         #:port 8080))

(with-handlers ([exn:break? (λ (e) (stop))])
  (sync/enable-break never-evt))

