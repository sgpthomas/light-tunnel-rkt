#lang racket/gui

(require racket/gui/base)

(provide create-strip
         set-color!
         render!
         rgb)

(struct strip (lights size canvas))

(define (convert num)
  (define tmp (inexact->exact (round num)))
  (cond [(< tmp 0) 0]
        [(< 255 tmp) 255]
        [else tmp]))

(define (rgb r g b)
  (make-object color% (convert r) (convert g) (convert b)))

(define (create-strip pin number #:brightness [brightness 255])
  (define toplevel
    (new frame%
         [label "Lights Simulation"]
         [width 1000]
         [height 50]))

  (define canvas
    (new canvas%
         [parent toplevel]))

  (send toplevel show #t)

  (strip (make-vector number (rgb 0 0 0)) number canvas))

(define (set-color! strip index color)
  (when (and (< index (strip-size strip))
             (<= 0 index))
    (vector-set! (strip-lights strip) index color)))

(define (render! strip)
  (define-values (win-w-2 win-h-2) (send (strip-canvas strip) get-scaled-client-size))
  (define-values (win-w win-h) (values (/ win-w-2 2)
                                       win-h-2))
  (define dc (send (strip-canvas strip) get-dc))
  (define box-w (/ win-w (strip-size strip)))
  (for ([i (in-range (strip-size strip))])
    (send dc set-brush (vector-ref (strip-lights strip) i) 'solid)
    (send dc set-pen "black" 1 'solid)
    (send dc draw-rectangle (* i box-w) 0 (/ box-w 1) win-h))
  (sleep 0.005))

(define strip (create-strip 10 150))
(define worker
  (thread
   (lambda ()
     (define (loop t)
       (for ([i 150])
         (define r (* (sin (* 6.14 (- t i))) 255))
         (set-color! strip i (rgb r 0 0)))
       (render! strip)
       (loop (add1 t))
       )
     (loop 0))))

(kill-thread worker)



