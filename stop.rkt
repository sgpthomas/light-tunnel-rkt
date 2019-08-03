#lang racket/base

(require "syntax.rkt"
         "client.rkt")

(send/mood ("raspberrypi.local" 1234 18 150)
           [(dim 0) (0 0 0) 0])
