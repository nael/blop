#lang racket/gui

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         racket/gui ffi/cvector
         
         "assets.rkt"
         )
(require "utils.rkt" "lvl.rkt" "render.rkt" "game.rkt" "math.rkt" "gui.rkt")


(define (make-scene)
 (gfx-node (constant-lin (vec 0 0) 1 0) null));k(gfx-image 'fx/ch1/well-bg))))

(define s (make-scene))

(require profile)

(define (init-view)
  ;(profile-thunk (lambda ()
                   (load-board s 'ch1/well)
                   (propagate-event 'enter));)qqqq
  ;(exit))
(define (timers t)
  (exec-timers-before t))

(parameterize ([uncaught-exception-handler default-exn-handler])
  (make-view s init-view propagate-event timers))
(define-namespace-anchor blop-top-level-ns)
(parameterize ([current-namespace (namespace-anchor->namespace blop-top-level-ns)])
  (graphical-read-eval-print-loop))