#lang racket/gui

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         racket/gui ffi/cvector
         )
(require "math.rkt" "assets.rkt")

; a simple render tree
(struct gfx-node ((transform #:mutable)
                  (children #:mutable)))
(struct gfx-drawable gfx-node ((image #:mutable) (frame #:mutable) (depth #:mutable) (colorize #:mutable #:auto)))
(define (constant-lin t s a)
  (lin (constant t) (constant s) (constant a)))
(define (gfx-image image) (gfx-drawable (constant-lin (vec 0 0) 1 0) null image (constant 0) (constant 0)))
(define (constant u)
  (if (lin? u) (constant-lin (lin-tr u) (lin-scale u) (lin-angle u)) (lambda x u)))
; ==
(define (gfx-node-add n c)
  (set-gfx-node-children! n (cons c (gfx-node-children n))))
(define (gfx-node-remove n c)
  (set-gfx-node-children! n (remove c (gfx-node-children n))))
(define (gfx-node-remove-all n cs)
  (for-each ((curry gfx-node-remove) n) cs))
(define draw-depth-mode #f);todo make this clean way

(define (rad->deg r) (* r (/ 180 pi)))



(define (draw-image image-name colorize frame l d)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glScalef (/ 2 WIDTH) (/ 2 HEIGHT) 1)
  (glTranslatef (- (/ WIDTH 2)) (- (/ HEIGHT 2)) 0)
  (define image (texture-by-name image-name))
  (glScalef (lin-scale l) (lin-scale l) 1)
  (glTranslatef (vec-x (lin-tr l)) (vec-y (lin-tr l)) 0)
  (glRotatef (rad->deg (lin-angle l)) 0 0 1)

  (glScalef (texture-w image) (texture-h image) 1)
  ;(and (<= d 10) (>= d 1) draw-depth-mode)
  (glBindTexture GL_TEXTURE_2D (texture-id image))
  (cond [colorize (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_COMBINE)
                  (define c (exact->inexact (- 1 (/ (- d 1) 9))))
                  ;              (printf "DEPTH: ~a ~a\n" c dd)
                  (glTexEnvfv GL_TEXTURE_ENV GL_TEXTURE_ENV_COLOR (list->gl-float-vector colorize))
                  
                  (glTexEnvi GL_TEXTURE_ENV GL_COMBINE_RGB GL_REPLACE)
                  (glTexEnvi GL_TEXTURE_ENV GL_SOURCE0_RGB GL_CONSTANT)
                  (glTexEnvi GL_TEXTURE_ENV GL_SOURCE1_RGB GL_CONSTANT)
                  (glTexEnvi GL_TEXTURE_ENV GL_OPERAND0_RGB GL_SRC_COLOR)
                  (glTexEnvi GL_TEXTURE_ENV GL_OPERAND1_RGB GL_SRC_COLOR)
                  
                  (glTexEnvi GL_TEXTURE_ENV GL_COMBINE_ALPHA GL_REPLACE)
                  (glTexEnvi GL_TEXTURE_ENV GL_SOURCE0_ALPHA GL_TEXTURE0)
                  (glTexEnvi GL_TEXTURE_ENV GL_SOURCE1_ALPHA GL_TEXTURE0)
                  (glTexEnvi GL_TEXTURE_ENV GL_OPERAND0_ALPHA GL_SRC_ALPHA)
                  (glTexEnvi GL_TEXTURE_ENV GL_OPERAND1_ALPHA GL_SRC_ALPHA)]
        
        [else (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)])
  
  
  ;(glColor4f 0 255 0 255)
  (let* ([sx (texture-x-coord image frame)]
         [sy (texture-y-coord image frame)]
         [tx1 (* sx (texture-sw image))]
         [ty1 (* sy (texture-sh image))]
         [tx2 (+ tx1 (texture-sw image)) ]
         [ty2 (+ ty1 (texture-sh image))])
      (if (texture-x-flip? image)
          (draw-quad tx2 ty1 tx1 ty2)
          (draw-quad tx1 ty1 tx2 ty2))))
(define (set-gfx-node-scale! n s)
  (set-gfx-node-transform! n (struct-copy lin (gfx-node-transform n) [scale s])))
(define (set-gfx-node-translation! n t)
  (set-gfx-node-transform! n (struct-copy lin (gfx-node-transform n) [tr t])))
(define (lin-eval l t)
  (lin ((lin-tr l) t) ((lin-scale l) t) ((lin-angle l) t)))
(define (render-frame scene at-time)
  ; this returns a list of lambdas rendering a specific image and its required depth
  (define (render-node node ltr)
    (let ([cur (lin-compose ltr (lin-eval (gfx-node-transform node) at-time))])
      (append* (if (and (gfx-drawable? node) (not (null? (gfx-drawable-image node))))
                   (list (cons ((gfx-drawable-depth node) at-time)
                               (lambda () (draw-image (gfx-drawable-image node)
                                                      (gfx-drawable-colorize node)
                                                      ((gfx-drawable-frame node) at-time)
                                                      cur
                                                      ((gfx-drawable-depth node) at-time)))))
                   (list))
               (for/list ([c (gfx-node-children node)])
                         (render-node c cur)))))
  (let ([draw-calls (render-node scene (lin (vec 0 0) 1 0))])
    (for ([c (sort draw-calls (lambda (c1 c2) (> (car c1) (car c2))))]) ((cdr c)))))
    
(define (ce)
  (let ([e (glGetError)])
    (when (not (equal? e 0))
      (displayln e))))
(define (draw-quad tx1 ty1 tx2 ty2)
  (glBegin GL_QUADS)
  (glTexCoord2d tx1 ty2)
  (glVertex3d 0 0 0.0)
  (glTexCoord2d tx2 ty2)
  (glVertex3d 1 0 0.0)
  (glTexCoord2d tx2 ty1)
  (glVertex3d 1 1 0.0)
  (glTexCoord2d tx1 ty1)
  (glVertex3d 0 1 0.0)
  (glEnd))

(define (resize w h)
  (unless (and (equal? w WIDTH) (equal? h HEIGHT))
    (glViewport 0 0 w h)
    (let ([aspect (/ w h)])
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glOrtho 0 w 0 h 1.0 -1.0)
      #t)))

(define (draw-opengl scene)
  (glClearColor 1 0 0 1)
  (glClear GL_COLOR_BUFFER_BIT)
;  (glEnable GL_DEPTH_TEST)
;  (glDepthMask GL_TRUE)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;(glBlendFunc GL_ONE GL_ZE
  (render-frame scene (current-inexact-milliseconds))
  (ce))
(define (init-opengl) '())

(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    (init-field scene)
    (init-field init-cb)
    (init-field event-cb)
    (init-field timer-cb)
    (define init? #f)
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless init?
           (init-cb)
           (set! init? #t))
         (timer-cb (current-inexact-milliseconds))
         (draw-opengl scene)
         (swap-gl-buffers)
         (sleep/yield 0.01)
         (queue-callback (lambda () (refresh)) #f))))
    
    (define/override (on-char key-event)
      (display "key evt ")
      (displayln key-event)
      (when (equal? (send key-event get-key-code) #\q ) (custodian-shutdown-all (current-custodian)))
      (when (equal? (send key-event get-key-code) #\d )(set! draw-depth-mode (not draw-depth-mode))))
    (define/override (on-event mouse-event)
      (when (send mouse-event button-up? 'left)
        (event-cb (list 'mouse-click (send mouse-event get-x) (- HEIGHT (send mouse-event get-y))))))
      ;(printf "mouse evt : ~a ~a\n" (send mouse-event get-x) (send mouse-event get-y)))
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
          (resize width height))))
    
    (super-instantiate () (style '(gl)))))
(define WIDTH 1400)
(define HEIGHT 700)
;(define-values (WIDTH HEIGHTX) (get-display-size))
;(define HEIGHT (+ HEIGHTX 60))
(define (make-view s init-cb event-cb timer-cb)
  (define win (new frame% (label "$") (min-width WIDTH) (min-height HEIGHT) (style (list 'no-resize-border 'float 'hide-menu-bar #|'no-caption|#))))
  (define gl  (new my-canvas% [parent win] [scene s] [init-cb init-cb] [event-cb event-cb] [timer-cb timer-cb]))
  (send win show #t))

(provide WIDTH HEIGHT constant-lin set-gfx-node-scale! set-gfx-node-translation! gfx-node-add constant make-view gfx-image  lin-eval)
(provide (struct-out gfx-node) (struct-out gfx-drawable) gfx-node-remove gfx-node-remove-all)
#|(provide (contract-out 
          [struct gfx-node ([transform lin?] [children list?])]
          [struct gfx-drawable ([transform lin?] [children list?] [image symbol?] [frame any/c] [depth any/c] [colorize any/c])]))|#
