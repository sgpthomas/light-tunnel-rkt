#lang racket/base
(require web-server/servlet
         web-server/servlet-env
         racket/match)

(define ns (make-base-namespace))

(define worker
  (thread
   (lambda ()
     (let loop ()
       (match (thread-receive)
         [x (println (eval (read (open-input-string x)) ns))]
         ['stop (kill-thread (current-thread))])
       (loop)))))

(define (my-app req)
  (define prog (bytes->string/locale (request-post-data/raw req)))
  (thread-send worker prog)
  ;; (eval (read (open-input-string prog)))
  (response/xexpr
   `(html)))

(serve/servlet my-app
               #:servlet-path "/main"
               #:command-line? #t)

