#lang racket

(require "render.rkt" "assets.rkt" "math.rkt")

(struct background (image-name))
(struct board-data (background things))
(struct thing (name
               node
               (walk-dest #:mutable #:auto)))

(define (things . l)
  (map (lambda (tn)
         (thing tn
                (gfx-node (constant-lin (vec 100 100) 1 0) (list (gfx-node (constant-lin (vec 0 0) 1 0) (list (gfx-image null)))))))
       l))
(define boards (make-hash))
(define event-handlers '())
(define (thing-drawable t)
  (car (gfx-node-children (car (gfx-node-children (thing-node t))))))
(define (register-event matcher handler)
  (define (h evt)
    (when (matcher evt) (handler evt)))
  (set! event-handlers (cons h event-handlers)))
(define (propagate-event e)
  (for ([h event-handlers]) (h e)))
(define (board name . elements)
  (define bg #f)
  (define things (make-hash))
  (define (populate es)
    (for ([e es])
      (cond [(background? e) (if (equal? bg #f)
                                 (set! bg (background-image-name e))
                                 (error name "more than one background"))]
            [(thing? e) (hash-set! things (thing-name e) e)]
            [(list? e) (populate e)]
            [else (error name "unknown element : ~a" e)])))
  (populate elements)
  (hash-set! boards name (board-data bg things)))
; modify the root node sc to scale and offset so that the background is maxed
; then returns a background node with the image
(define (make-bg-node sc img)
  (let* ([tex (texture-by-name img)]
         [bg-w (texture-w tex)] [bg-h (texture-h tex)])
    (define scale (exact->inexact (min (/ WIDTH bg-w) (/ HEIGHT bg-h))))
    ;(printf "Dim ~a\n" scale)
    (define mid-x (/ (- (/ WIDTH scale) bg-w) 2))
    (define mid-y (/ (- (/ HEIGHT scale) bg-h) 2))
    (let ([tr (lin (vec mid-x mid-y) scale 0)])
      (printf "pp ~a\n" tr)
      (set-gfx-node-transform! sc (constant tr))
      (gfx-drawable (constant-lin (vec 0 0) 1 0) (list) img (constant 0) (constant 255)))))
(define current-scene #f)
(define (load-board s bname)
  (set! current-board-name bname)
  (set! current-scene s)
  (define b (hash-ref boards bname))
  (unless (equal? (board-data-background b) #f) (gfx-node-add s (make-bg-node s (board-data-background b))))
  (for ([(_ t) (board-data-things b)])
    (gfx-node-add s (thing-node t))))

(define (animate! node start-time rate begin end)
  (define (frame time)
    (let ([f (modulo (+ begin (round (* (- time start-time) rate))) end)])
      f
      ))
  (set-gfx-drawable-frame! node (if (equal? begin end) (constant begin) frame)))
(define (set-tex! node tex)
  (set-gfx-drawable-image! node tex))
(define (set-constant-depth! node d)
  (set-gfx-drawable-depth! node (lambda (_) d)))
(define (layer ln d imgname)
  (define n (gfx-image imgname))
  (set-gfx-drawable-depth! n (constant d))
  (thing ln n))
(board 'ch1/well
       (background 'fx/ch1/well/horiz)
       (layer 'lm1 19 'fx/ch1/well-dm)
       (layer 'l0 20 'fx/ch1/well/maisons)
       (layer 'l1 9 'fx/ch1/well/canap)
       (layer 'l2 8 'fx/ch1/well/serre)
       (layer 'l3 7 'fx/ch1/well/ferme)
       (layer 'l4 6 'fx/ch1/well/coffre)
       (layer 'l9 5.9 'fx/ch1/well/coffre-ouvert)
       (layer 'l5 2.5 'fx/ch1/well/wellwell)
       (layer 'l6 1 'fx/ch1/well/cloture)
       (things 'bob 'bill))


(define current-board-name 'none)
(define (current-board) (hash-ref boards current-board-name))
(define (thing-by-name name)
  (hash-ref (board-data-things (current-board)) name))

(define (start-anim thing-name anim)
  (let* ([t (thing-by-name thing-name)]
         [n (thing-drawable t)]
         [boundaries (texture-begin-end anim)])
    (set-tex! n anim)
    (set-gfx-node-transform! n (constant (texture-transform anim)))
    (animate! n (current-inexact-milliseconds) 0.008 (car boundaries) (cdr boundaries))))

(define (straight-move-to! start-time n start-pos dest speed)
  (let* ([dl (normalize (vec- dest (lin-tr start-pos)))]
         [maxd (norm (vec- dest (lin-tr start-pos)))])
    (define (tr t)
      (let ([d (min (* (- t start-time) speed) maxd)])
        (vec+ (lin-tr start-pos) (vec* d dl))))
    (set-gfx-node-translation! n tr)
    (+ start-time (/ maxd speed))))
; an action is a fun next -> time -> ()
; it is supposed to call next when it is completed
; which is not nescessarily on the same call stack (maybe at the end of some animation or w/e)
(define (((par . actions) next) time)
  (define act-finished 0)
  (define act-count (length actions))
  (define (end-callback t)
    (set! act-finished (add1 act-finished)) ; todo race condition, no MT now but could be
    (when (equal? act-count act-finished)
      (next t)))
  (for ([a actions])
    ((a end-callback) time)))
(define (do-action a t)
  ((a (lambda (t) '())) t))
(define (((straight-move-to thing-name walk-anims dest speed) next) time)
  (let* ([thing (thing-by-name thing-name)]
         [node (thing-node thing)]
         [start-pos (lin-eval (gfx-node-transform node) time)]
         [anim (choose-anim walk-anims (lin-tr start-pos) dest)]
         [walk-anim (car anim)] [still-anim (cdr anim)]
         [finish-time (straight-move-to! time node start-pos dest speed)])
    (start-anim thing-name walk-anim)
    (define (stop-walk t)
      (when (equal? (thing-walk-dest thing) dest) (start-anim thing-name still-anim))
      (next t))
    (set-thing-walk-dest! thing dest)
    (register-timer finish-time stop-walk)))

; walk anims : '((dir1 . anim1) (dir2 . anim2) ...)
; directions must be normalized
(define (choose-anim anims start dest)
  (let ([d (vec- dest start)])
    (cdr (argmax (lambda (c) (vec-dot (car c) d)) anims))))

(define timers '())
(define (register-timer at-time fun)
  (set! timers (cons (cons at-time fun) timers)))
(define (exec-timers-before at)
  (let-values ([(x nx) (partition (lambda (t) (<= (car t) at)) timers)])
    (for-each (lambda (t) ((cdr t) at)) x)
    (set! timers nx)))
(register-event (lambda (_) #t) (lambda (e) (printf "event: ~a\n" e)))
(define (viewport->scene t u)
  (lin-apply (lin-inverse (lin-eval (gfx-node-transform current-scene) t)) u))

                    
(define walka (list (cons (vec -1 0) (cons 'fx/bob-walk-left 'fx/bob-still-left)) (cons (vec 1 0) (cons 'fx/bob-walk-right 'fx/bob-still-right))))
(register-event (lambda (e) (and (pair? e) (equal? 'mouse-click (car e))))
                (lambda (e)
                  (do-action
                   (straight-move-to 'bob walka (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))) 0.2)
                   (current-inexact-milliseconds)))
                  ;(define p (viewport->scene  (current-inexact-milliseconds))) (vec (cadr e) (caddr e))))
                  
                  ;(printf "GPX ~a ~a\n" p (get-pixel 'fx/ch1/well-dm (vec->int-vec p))))
                  ;(((straight-move-to 'bob walka (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))) 0.2)
                  ; (lambda (t) '())) (current-inexact-milliseconds))
                  ;(define a (straight-move-to 'bob (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))) 0.2))
                 ;((a (lambda (t) (start-anim 'bob 'fx/bob-still))) (current-inexact-milliseconds))
                  )
(define (byte->meters b near far)
  (exact->inexact (+ near (* (- 1 (/ (+ b 1) 256)) (- far near)))))
(define (auto-depth! n depth-map)
  (define (d t)
    (define pix (cadr (get-pixel depth-map (vec->int-vec ((lin-tr (gfx-node-transform n)) t)))))
    (define s (+ (* 9 (- 1 (/ pix 255))) 1) )
    s)
  (set-gfx-drawable-depth! (car (gfx-node-children (car (gfx-node-children n)))) d))


(register-event (lambda (e) (equal? e 'enter ))
                (lambda (e)
                  (define n (thing-node (thing-by-name 'bob)))
                  (auto-depth! n 'fx/ch1/well-dm)
                  (define (u t)
                    
                    (define s (byte->meters (cadr (get-pixel 'fx/ch1/well-dm (vec->int-vec ((lin-tr (gfx-node-transform n)) t)))) 1 2))

                    ;(printf "S:~a\n" s)
                    (/ 1 s)
                    )
                  (start-anim 'bob 'fx/bob-still)
                 (set-gfx-node-scale! (car (gfx-node-children n)) u)
                  e
                 ))
                  
(provide thing-by-name load-board propagate-event exec-timers-before)
