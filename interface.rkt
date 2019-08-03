#lang racket
(require racket/draw)
(provide (struct-out strip)
         interface/init)

(struct strip (name))

(strip "hi")

(define (interface/init)
  (void))

()

