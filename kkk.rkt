#lang racket
(define (f td ts)
  (define a (/ td ts))
  (define s (sqrt (/ (round a) a)))
  (exact->inexact (/ (ceiling (/ td s)) (floor (* ts s)))))