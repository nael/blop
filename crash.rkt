#lang racket
(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         racket/gui ffi/cvector
         "assets.rkt"
         )
(require "lvl.rkt" "render.rkt" "game.rkt" "math.rkt")

(define (make-scene)
 (gfx-node (constant-lin (vec 0 0) 1 0) (list)))

(define s (make-scene))
(define (init-view)
    (for ([i (in-range 200)]) (load-tex (list "ass/bg.png")) (printf "done\n")))
(make-view s init-view (lambda (e) #f) (lambda (t) #f))