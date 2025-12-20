#lang racket
(require crypto
         crypto/argon2
         threading
         net/url
         (prefix-in files:     web-server/dispatchers/dispatch-files)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter:    web-server/dispatchers/dispatch-filter)
         (prefix-in logresp:   web-server/dispatchers/dispatch-logresp)
         (prefix-in lift:      web-server/dispatchers/dispatch-lift)
         web-server/dispatchers/filesystem-map
         web-server/web-server
         
         "util/db.rkt"
         "config.rkt"
         "util/session.rkt"
         "util/misc.rkt"
         "handlers/user.rkt"
         "handlers/errors.rkt"
         "models/user.rkt"
         "i18n/utils.rkt")

(provide start)

(crypto-factories argon2-factory)

(define files
  (~> (make-url->path "static")
      (compose (λ (u) (struct-copy url u [path (cdr (url-path u))])))
      (files:make #:url->path _)
      (filter:make #rx"^/static/" _)))

(define dispatcher
  (~> (sequencer:make files
                      (lift:make user-servlet))
      error-dispatcher
      session-dispatcher
      locale-dispatcher
      contextualizing-dispatcher
      (logresp:make #:log-path (current-output-port)
                    #:format 'apache-default)))
      
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

;; creates a new custodian, calls f with it set as current
;; and returns a function that kills it
(define (with-stop f)
  (parameterize ([current-custodian (make-custodian)])
    (let ([stop (f)]
          [cust (current-custodian)])
      (λ () (stop) (custodian-shutdown-all cust)))))

(define (with-environment f)
  (~> f
      with-connection-from-config
      thunk with-config-from-command-line
      thunk with-stop))

(define (start)
  (with-environment
    (λ ()
      (displayln "Starting web server...")
      (start-user-services)
      (serve #:dispatch dispatcher
             #:port 8080))))
