#lang racket/base

(require "syntax.rkt")

(send/mood ("raspberrypi.local" 1234 18 450)
           [(wave 10 0.11 0) (0.8 0.3 0.5) -0.3]
           [(wave 10 0.11 0) (0.5 0.3 0.8) 0.15]
           [(wave 9 0.1 100) (0.1 0.8 0.1) 0.15]

           [(wave 100 0.1 0) (0.4 0.4 0.4) 0.3])
