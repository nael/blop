#lang racket

(struct vec (x y) #:transparent)


; represents u |-> scale*(tr + exp(i*angle)*u)
(struct lin (tr scale angle) #:transparent)

(define (vec+ u v) (vec (+ (vec-x u) (vec-x v)) (+ (vec-y u) (vec-y v))))
(define (vec- u v) (vec (- (vec-x u) (vec-x v)) (- (vec-y u) (vec-y v))))
(define (norm v) (sqrt (+ (* (vec-x v) (vec-x v)) (* (vec-y v) (vec-y v)))))
(define (normalize v) (if (equal? v (vec 0 0)) v (vec* (/ 1 (norm v)) v)))
(define (vec* s u) (vec (* s (vec-x u)) (* s (vec-y u))))
(define (rotate u theta)
  (let ([c (cos theta)] [s (sin theta)])
    (vec (- (* (vec-x u) c) (* (vec-y u) s))
         (+ (* (vec-x u) s) (* (vec-y u) c)))))
(define (lin-compose f g)
  (lin (vec+ (vec* (/ 1 (lin-scale g)) (lin-tr f)) (rotate (lin-tr g) (lin-angle f)))
       (* (lin-scale f) (lin-scale g))
       (+ (lin-angle f) (lin-angle g))))
(define (lin-apply f u)
  (vec* (lin-scale f) (vec+ (lin-tr f) (rotate u (lin-angle f)))))
(define (lin-inverse f)
  (lin-compose (lin-rotation (- (lin-angle f))) ; too tired to compute
               (lin-compose (lin-translation (vec* -1 (lin-tr f)))
                            (lin-hom (/ 1 (lin-scale f))))))
(define lin-id (lin (vec 0 0) 1 0))
(define (lin-rotation t) (lin (vec 0 0) 1 t))
(define (lin-translation u) (lin u 1 0))
(define (lin-hom l) (lin (vec 0 0) l 0))
(define (vec->int-vec u)
  (vec (inexact->exact (round (vec-x u))) (inexact->exact (round (vec-y u)))))
(provide (struct-out vec) (struct-out lin) lin-apply lin-id norm normalize vec* vec+ vec- rotate lin-compose vec->int-vec lin-inverse)