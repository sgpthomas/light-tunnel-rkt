#lang racket/base
(require simple-http
         racket/format)

(provide send-quote)

(define (send-quote host port data)
  (define requester
    (update-port
     (update-host json-requester host)
     port))

  (with-handlers ([exn:fail:network:http:read? displayln])
    (post requester "/put" #:data (~a data))))
