#lang racket/gui

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         racket/gui ffi/cvector
         "assets.rkt"
         )
(require "lvl.rkt" "render.rkt" "game.rkt" "math.rkt" "gui.rkt")


(define (make-scene)
 (gfx-node (constant-lin (vec 0 0) 1 0) null));k(gfx-image 'fx/ch1/well-bg))))

(define s (make-scene))

(define (init-view)
  (load-board s 'ch1/well)
  (propagate-event 'enter))
(define (timers t)
  (exec-timers-before t))
(make-view s init-view propagate-event timers)
(define-namespace-anchor blop-top-level-ns)
(parameterize ([current-namespace (namespace-anchor->namespace blop-top-level-ns)])
  (graphical-read-eval-print-loop))