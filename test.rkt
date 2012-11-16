#lang racket/gui

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         racket/gui ffi/cvector
         "assets.rkt"
         )
(require "lvl.rkt" "render.rkt" "game.rkt" "math.rkt")


(define (make-scene)
 (gfx-node (constant-lin (vec 0 0) 1 0) (list)))

(define s (make-scene))

;(steady-rotation! s (current-inexact-milliseconds) 0.001)
;(define bobn (car (gfx-node-children (car (gfx-node-children s)))))
;(animate! bobn (current-inexact-milliseconds) 0.01 0 9)
;(set-constant-depth! bobn -0.3)
(define (init-view)
  (load-board s 'ch1/well)
  (propagate-event 'enter))
(define (timers t)
  (exec-timers-before t))
(make-view s init-view propagate-event timers)