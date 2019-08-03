#lang racket/base

(require "client.rkt"
         racket/math
         (for-syntax racket/base
                     syntax/parse))
(provide send/mood)

(define-syntax (send/mood stx)
  (define-syntax-class phrase
    #:attributes (fun)
    #:datum-literals (wave shimmer gim gauss)
    (pattern (wave period amp phase)
             #:with fun #'(lambda (num data t i)
                            (values
                             (* (* 255.0 amp) (sin (+ (* 100 2 3.1415 period (- t i)) phase)))
                             data)))

    (pattern (shimmer low up)
             #:with fun #'(let ([c (random low up)])
                            (lambda (num data t i) (values c data))))

    (pattern (dim val)
             #:with fun #'(lambda (num data t i) (values val data)))

    (pattern (gauss width speed amp phase)
             #:with fun #'(let ([fact 1000])
                            (lambda (num data t i)
                              (define p (abs (cos (* (/ (* 2 pi) (* fact speed))
                                                     (- t phase)))))
                              (define data-p
                                (match data
                                  [#f (random 1 num)]
                                  [center
                                   (if (= (modulo
                                           (exact-truncate t)
                                           (exact-truncate (* (/ 1 2) speed fact)))
                                          (exact-truncate phase))
                                       (random 1 num)
                                       center)]))
                              (define a amp)
                              ;; (define w (+ 10 (* (* 10 width) (- 1 (sqr p)))))
                              (define w width)
                              (values (max
                                       0
                                       (- (* a
                                            (exp (- (/ (sqr (- i data-p))
                                                       (* 2 (sqr w))))))
                                          (* 100 p)))
                                      data-p))))
    )

  (syntax-parse stx
    [(_ (host port pin num)
        [body:phrase (red green blue) factor] ...
        )

     #'(send-quote host port
                   '(let* ([strip (create-strip pin num)])
                      (struct col (r g b v))
                      (struct element (data fun col))
                      (define elems
                        (list (element #f body.fun (col red green blue factor)) ...))
                      (define (loop elems t)
                        (define elems-p
                          (foldl (lambda (i elems)
                                   (define elems-p
                                     (map (lambda (e)
                                            (let*-values
                                                ([(c) (element-col e)]
                                                 [(data) (element-data e)]
                                                 [(fun) (element-fun e)]
                                                 [(val data-p) (fun num data (* (col-v c) t) i)]
                                                 [(r) (* (* 1.0 (col-r c)) val)]
                                                 [(g) (* (* 1.0 (col-g c)) val)]
                                                 [(b) (* (* 1.0 (col-b c)) val)]
                                                 [(col-p) (col r g b (col-v c))])
                                              (cons (element data-p fun c) col-p)))
                                          elems))
                                   (define c
                                     (foldl (lambda (c acc)
                                              (let* ([r (+ (col-r c) (col-r acc))]
                                                     [g (+ (col-g c) (col-g acc))]
                                                     [b (+ (col-b c) (col-b acc))])
                                                (col r g b 0)))
                                            (col 0 0 0 0)
                                            (map cdr elems-p)))
                                   (set-color! strip i (rgb (col-r c) (col-g c) (col-b c)))
                                   (map car elems-p))
                                 elems
                                 (build-list num values)))
                        (render! strip)
                        (loop elems-p (add1 t)))
                      (loop elems 0)))]))
