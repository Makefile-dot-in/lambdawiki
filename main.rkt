#lang racket
(require threading
         net/url
         (prefix-in files:     web-server/dispatchers/dispatch-files)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter:    web-server/dispatchers/dispatch-filter)
         web-server/dispatchers/filesystem-map
         web-server/servlet-env
         web-server/web-server)

(define files
  (~> (make-url->path "static")
      (compose (λ (u) (struct-copy url u [path (cdr (url-path u))])))
      (files:make #:url->path _)
      (filter:make #rx"^/static/" _)))

(define stop
  (serve #:dispatch (sequencer:make files)
         #:port 8080))

(with-handlers ([exn:break? (λ (e) (stop))])
  (sync/enable-break never-evt))

