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
       ;(layer 'lm1 19 'fx/ch1/well-background)
       (layer 'l0 20 'fx/ch1/well/maisons)
       (layer 'l1 9.5 'fx/ch1/well/canap)
       (layer 'l2 9 'fx/ch1/well/serre)
       (layer 'l3 9 'fx/ch1/well/ferme)
       (layer 'l4 6 'fx/ch1/well/coffre)
       (layer 'l9 5.9 'fx/ch1/well/coffre-ouvert)
       (layer 'l5 2.8 'fx/ch1/well/wellwell)
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
(define (start-or-continue-anim thing-name anim)
  (unless (equal? anim (gfx-drawable-image (thing-drawable (thing-by-name thing-name))))
    (start-anim thing-name anim)))
(define (straight-move-to! start-time n start-pos dest speed)
  (if (equal? (lin-tr start-pos) dest)
      (begin (set-gfx-node-translation! n (constant (lin-tr start-pos))) start-time)
      (let* ([dl (normalize (vec- dest (lin-tr start-pos)))]
             [maxd (norm (vec- dest (lin-tr start-pos)))])
        (define (tr t)
          (let ([d (min (* (- t start-time) speed) maxd)])
            (vec+ (lin-tr start-pos) (vec* d dl))))
        (set-gfx-node-translation! n tr)
        (+ start-time (/ maxd speed)))))
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
(define (((straight-move-to thing-name walk-anims dest speed stop?) next) time)
  (let* ([thing (thing-by-name thing-name)]
         [node (thing-node thing)]
         [start-pos (lin-eval (gfx-node-transform node) time)]
         [anim (choose-anim walk-anims (lin-tr start-pos) dest)]
         [walk-anim (car anim)] [still-anim (cdr anim)]
         [finish-time (straight-move-to! time node start-pos dest speed)])
    (start-or-continue-anim thing-name walk-anim)
    (define (stop-walk t)
      (printf "stopwalk called\n")
      (when (equal? (thing-walk-dest thing) dest)
        (when stop? (start-anim thing-name still-anim))
        (next t)))
    (set-thing-walk-dest! thing dest)
    (printf "Registering stop walk timer in ~a ms\n" (- (current-inexact-milliseconds) finish-time))
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
    (set! timers nx) ; do it now in case someone wants to register a timer in a timer
    (for-each (lambda (t) ((cdr t) at)) x)))
(register-event (lambda (_) #t) (lambda (e) (printf "event: ~a\n" e)))
(define (viewport->scene t u)
  (lin-apply (lin-inverse (lin-eval (gfx-node-transform current-scene) t)) u))

(define (colorize! thing-name color)
  (set-gfx-drawable-colorize! (thing-drawable (thing-by-name thing-name)) color))

(define (walk-straight thing-name dest)
  (straight-move-to thing-name walka dest 0.2 #f))
(define (walk-straight-and-stop thing-name dest)
  (straight-move-to thing-name walka dest 0.2 #t))

(define (((seq-list acts) next) t)
  (cond [(null? acts) (next t)]
        [else (((car acts) ((seq-list (cdr acts)) next)) t)]))
(define (seq . acts) (seq-list acts))
(define ((do-nothing next) t) #f)
(define walka (list (cons (vec -1 0) (cons 'fx/bob-walk-left 'fx/bob-still-left)) (cons (vec 1 0) (cons 'fx/bob-walk-right 'fx/bob-still-right))))
(define dl 50)
(define (split-vec u)
  (if (equal? (norm u) 0) u
      (vec* (min 1 (/ dl (norm u))) u)))
(define (split-path-seg u v)
  (define d (vec- v u))
  (define sv (split-vec d))
  (if (equal? sv d)
      (list u v)
      (cons u (split-path-seg (vec+ u sv) v))))
(define (split-path path)
  (remove-duplicates (cond [(null? path) null]
                           [(null? (cdr path)) path]
                           [else (append (split-path-seg (car path) (cadr path)) (split-path (cdr path)))])))
  
(define (check-line img start end)
  (define u (vec- end start))
  (define nu (normalize u))
  (define ntick (exact->inexact (ceiling (norm u))))
  (for/and ([t (in-range 0 ntick 1)])
    (zero? (car (get-pixel img (vec->int-vec (vec+ start (vec* t nu)))))))) ; warning no transfrom taken into account

(register-event (lambda (e)
                  (and (pair? e) (equal? 'mouse-click (car e))))
                (lambda (e)
                  (define real-endpos (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))))
                  (when (zero? (car (get-pixel 'fx/ch1/well/wellwell-cm (vec->int-vec real-endpos))))
                    (define endpos (grid-nearest main-r-grid real-endpos))
                    (define real-startpos ((lin-tr (gfx-node-transform (thing-node (thing-by-name 'bob)))) (current-inexact-milliseconds)))
                    (define startpos (grid-nearest main-r-grid real-startpos))
                    (define path (djs main-r-grid startpos endpos))
                    ;(for ([p path])
                    ;  (set-gfx-drawable-colorize! (hash-ref main-grid p) (list 0 255 0 255)))
                    (define (simplify-path from pt)
                      (cond [(null? pt) null]
                            [(check-line 'fx/ch1/well/wellwell-cm from (car pt)) (list (car pt))]
                            [else (cons (car pt) (simplify-path from (cdr pt)))]
                            ))
                  (define (enhance-p pt)
                    (define ept (split-path (cons (car pt) (reverse (simplify-path (car pt) (reverse (cdr pt)))))))
                    ept)
                    (define (eept pt)
                      (if (null? pt) null
                          (let ([ept (enhance-p pt)])
                            (cons (car ept) (eept (cdr ept))))))
                    (define (walk-path pt)
                      (if (null? pt)
                          do-nothing
                          (let ([ept (enhance-p pt)])
                            (seq (if (null? (cdr ept))
                                     (walk-straight-and-stop 'bob (car ept))
                                     (walk-straight 'bob (car ept)))
                                 (walk-path (cdr ept))))))
                    (define sqp (cons real-startpos (append path (list real-endpos))))
                    (define final-path sqp); (eept sqp))
                    ;(when main-grid (gfx-node-remove-all current-scene (hash-values main-grid)))
                    ;(set! main-grid (make-grid (remove-duplicates (append final-path (split-path sqp)))))
                    (do-action
                     ;(walk-straight 'bob (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))))
                     (walk-path final-path)
                     (current-inexact-milliseconds)))))
(define (byte->meters b near far)
  (exact->inexact (+ near (* (- 1 (/ (+ b 1) 256)) (- far near)))))
(define (auto-depth! n depth-map)
  (define (d t)
    (define pix (cadr (get-pixel depth-map (vec->int-vec ((lin-tr (gfx-node-transform n)) t)))))
    (define s (+ (* 9 (- 1 (/ pix 255))) 1) )
    s)
  (set-gfx-drawable-depth! (car (gfx-node-children (car (gfx-node-children n)))) d));todo ugly

(require "pf.rkt")

(define (make-grid positions)
  (define gns (make-hash))
  (for ([p positions])
    (define bn (gfx-image 'pixel))
    ;(set-gfx-node-scale! bn (constant 10))
    (set-gfx-node-translation! bn (constant p))
    (set-gfx-drawable-colorize! bn (list 255 0 0 255))
    (hash-set! gns p bn)
    (gfx-node-add current-scene bn))
  gns)
(define main-grid #f)
(define main-r-grid #f)
(register-event (lambda (e) (equal? e 'enter ))
                (lambda (e)
                  (define n (thing-node (thing-by-name 'bob)))
                  (auto-depth! n 'fx/ch1/well-dm)
                  (define (u t)
                    
                    (define s (byte->meters (cadr (get-pixel 'fx/ch1/well-dm (vec->int-vec ((lin-tr (gfx-node-transform n)) t)))) 1 1.5))

                    ;(printf "S:~a\n" s)
                    (/ 1 s))
                  (start-anim 'bob 'fx/bob-still)
                 (set-gfx-node-scale! (car (gfx-node-children n)) u)
                  (set! main-r-grid (build-grid-from-image 'fx/ch1/well/wellwell-cm))
                  ;(set! main-grid (make-grid (hash-keys main-r-grid)))
                 ))
                  
(provide thing-by-name load-board propagate-event exec-timers-before)
