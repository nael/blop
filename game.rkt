#lang racket

(require "render.rkt" "assets.rkt" "math.rkt" "pf.rkt" "utils.rkt")

; board definitions

(struct background (image-name))
(struct board-data (background things))
(struct thing (name
               node
               
               (walk-dest #:mutable #:auto)
               (cmap #:mutable #:auto)))

(define (things . l)
  (map (lambda (tn)
         (thing tn
                (gfx-node (constant-lin (vec 100 100) 1 0) (list (gfx-node (constant-lin (vec 0 0) 1 0) (list (gfx-image null)))))))
       l))
(define boards (make-hash))

(define (thing-drawable t)
  (car (gfx-node-children (car (gfx-node-children (thing-node t))))))

(define (viewport->scene t u)
  (lin-apply (lin-inverse (lin-eval (gfx-node-transform current-scene) t)) u))

(define (layer ln d imgname)
  (define n (gfx-image imgname))
  (set-gfx-drawable-depth! n (constant d))
  (define t (thing ln n))
  (define cmsym (string->symbol (format "~a-cm" (symbol->string imgname))))
  (when (image-exists? cmsym)
    (set-thing-cmap! t cmsym))
  t)

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
    (define mid-x 0);(/ (- (/ WIDTH scale) bg-w) 2))
    (define mid-y 0);(/ (- (/ HEIGHT scale) bg-h) 2))
    (let ([tr (lin (vec mid-x mid-y) scale 0)])
      (set-gfx-node-transform! sc (constant tr))
      (gfx-drawable (constant-lin (vec 0 0) 1 0) (list) img (constant 0) (constant 255)))))
(define (viewport-w)
  (image-w (board-data-background (current-board))))
(define (viewport-h)
  (image-h (board-data-background (current-board))))
(define current-scene #f)
(define current-board-name 'none)
(define (current-board) (hash-ref boards current-board-name))
(define (thing-by-name name)
  (hash-ref (board-data-things (current-board)) name))
(define (load-board s bname)
  (set! current-board-name bname)
  (set! current-scene s)
  (define b (hash-ref boards bname))
  (unless (equal? (board-data-background b) #f) (gfx-node-add s (make-bg-node s (board-data-background b))))
  (for ([(_ t) (board-data-things b)])
    (when (thing-cmap t)
      (set! current-cmaps (cons (build-cmap-from-image (thing-cmap t) 15) current-cmaps)))
    (gfx-node-add s (thing-node t))))



; node manipulation

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
(define (colorize! thing-name color)
  (set-gfx-drawable-colorize! (thing-drawable (thing-by-name thing-name)) color))
; returns the end time
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
; sets the depth of d from the position of n onto the depth-map
(define (auto-depth! n depth-map)
  (define (d t)
    (define pix (cadr (get-pixel depth-map (vec->int-vec ((lin-tr (gfx-node-transform n)) t)))))
    (define s (+ (* 9 (- 1 (/ pix 255))) 1) )
    s)
  (set-gfx-drawable-depth! (car (gfx-node-children (car (gfx-node-children n)))) d));todo ugly


; event handling

(define event-handlers '())
(define (register-event matcher handler)
  (define (h evt)
    (when (matcher evt) (handler evt)))
  (set! event-handlers (cons h event-handlers)))
(define (propagate-event e)
  (for ([h event-handlers]) (h e)))
(define timers '())
(define (register-timer at-time fun)
  (set! timers (cons (cons at-time fun) timers)))
(define (exec-timers-before at)
  (let-values ([(x nx) (partition (lambda (t) (<= (car t) at)) timers)])
    (set! timers nx) ; do it now in case someone wants to register a timer in a timer
    (for-each (lambda (t) ((cdr t) at)) x)))


(board 'ch1/well
       (background 'fx/ch1/well/bg)
       (layer 'lm1 21 'fx/ch1/well/horiz)
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

; actions, chaining ...

; an action is a fun next -> time -> ()
; it is supposed to call next when it is completed
; which is not nescessarily on the same call stack (maybe at the end of some animation or w/e)

; plays all the actions in parallel
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

; plays all the actions chained
(define (seq . acts) (seq-list acts))
(define (((seq-list acts) next) t)
  (cond [(null? acts) (next t)]
        [else (((car acts) ((seq-list (cdr acts)) next)) t)]))

(define ((do-nothing next) t) #f)



; moving, walking

(define (start-anim thing-name anim)
  (let* ([t (thing-by-name thing-name)]
         [n (thing-drawable t)]
         [boundaries (texture-begin-end anim)])
    (set-tex! n anim)
    ;(set-gfx-node-transform! n (constant (texture-transform anim)))
    (animate! n (current-inexact-milliseconds) 0.008 (car boundaries) (cdr boundaries))))
(define (start-or-continue-anim thing-name anim)
  (unless (equal? anim (gfx-drawable-image (thing-drawable (thing-by-name thing-name))))
    (start-anim thing-name anim)))

; walk animations : '((dir1 . anim1) (dir2 . anim2) ...)
; with normalized directions
(define (((straight-move-to thing-name walk-anims dest speed stop?) next) time)
  (let* ([thing (thing-by-name thing-name)]
         [node (thing-node thing)]
         [start-pos (lin-eval (gfx-node-transform node) time)]
         [anim (choose-anim walk-anims (lin-tr start-pos) dest)]
         [walk-anim (car anim)] [still-anim (cdr anim)]
         [finish-time (straight-move-to! time node start-pos dest speed)])
    (start-or-continue-anim thing-name walk-anim)
    (define (stop-walk t)
      (when (equal? (thing-walk-dest thing) dest)
        (when stop? (start-anim thing-name still-anim))
        (next t)))
    (set-thing-walk-dest! thing dest)
    (register-timer finish-time stop-walk)))
(define (choose-anim anims start dest)
  (let ([d (vec- dest start)])
    (cdr (argmax (lambda (c) (vec-dot (car c) d)) anims))))
(define (walk-straight thing-name dest)
  (straight-move-to thing-name walka dest 0.22 #f))
(define (walk-straight-and-stop thing-name dest)
  (straight-move-to thing-name walka dest 0.22 #t))


(define walka (list (cons (vec -1 0) (cons 'fx/bob-walk-left 'fx/bob-still-left)) (cons (vec 1 0) (cons 'fx/bob-walk-right 'fx/bob-still-right))))


; pathfinding

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
(define (is-walkable? pos)
  (for/and ([cm current-cmaps])
    (define e (lin-apply (lin-inverse (texture-transform (cmap-image cm))) pos))
    (zero? (cadddr (get-pixel (cmap-image cm) (vec->int-vec e))))))
(define (check-line img start end)
  (define u (vec- end start))
  (define nu (normalize u))
  (define ntick (exact->inexact (ceiling (norm u))))
  (for/and ([t (in-range 0 ntick 2)])
    (is-walkable? (vec+ start (vec* t nu)))))
    
(define current-cmaps null)

(define (walk-to thing-name real-endpos)
  (define graph (with-counter 'make-graph (lambda () (make-pf-graph 0 0 (viewport-w) (viewport-h) current-cmaps))))
  (draw-debug-grid 'pff (hash-keys graph))
  (define endpos (grid-nearest graph real-endpos))
  (define real-startpos ((lin-tr (gfx-node-transform (thing-node (thing-by-name thing-name)))) (current-inexact-milliseconds)))
  (define startpos (grid-nearest graph real-startpos))
  (define path (with-counter 'dijkstra (lambda () (djs graph startpos endpos))))
  ;(draw-debug-grid 'path path)
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
  (define final-path sqp)
  (with-counter 'path-simplify (lambda () (walk-path final-path))))

(register-event (lambda (e)
                  (and (pair? e) (equal? 'mouse-click (car e))))
                (lambda (e)
                  (define real-endpos (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))))
                  ;(when (zero? (car (get-pixel 'fx/ch1/well/wellwell-cm (vec->int-vec real-endpos))))
                  (when (is-walkable? real-endpos)
                    ; (eept sqp))
                    ;(when main-grid (gfx-node-remove-all current-scene (hash-values main-grid)))
                    ;(set! main-grid (make-grid (remove-duplicates (append final-path (split-path sqp)))))
                    (do-action
                     ;(walk-straight 'bob (viewport->scene  (current-inexact-milliseconds) (vec (cadr e) (caddr e))))
                     (with-counter 'pf (lambda () (walk-to 'bob real-endpos)))
                     (current-inexact-milliseconds)))
                    (printf "PF perfs =============\n")
                    (print-counters)))
(define (byte->meters b near far)
  (exact->inexact (+ near (* (- 1 (/ (+ b 1) 256)) (- far near)))))



; debugging

(define debug-things (make-hash))
(define (remove-debug name)
  (gfx-node-remove-all current-scene (hash-ref debug-things name)))
(define (draw-debug-grid name positions)
  (define gns (make-hash))
  (when (hash-has-key? debug-things name)
    (remove-debug name))
  (define nodes (for/list ([p positions])
    (define bn (gfx-image 'pixel))
    ;(set-gfx-node-scale! bn (constant 2))
    (set-gfx-node-translation! bn (constant p))
    (set-gfx-drawable-colorize! bn (list 0 0 255 255))
    (hash-set! gns p bn)
    (gfx-node-add current-scene bn)
    bn))
  (hash-set! debug-things name nodes))
(register-event (lambda (_) #t) (lambda (e) (printf "event: ~a\n" e)))

(define main-r-grid #f)
(register-event (lambda (e) (equal? e 'enter ))
                (lambda (e)
                  (define n (thing-node (thing-by-name 'bob)))
                  (auto-depth! n 'fx/ch1/well-dm)
                  (define (u t)
                    
                    (define s (byte->meters (cadr (get-pixel 'fx/ch1/well-dm (vec->int-vec ((lin-tr (gfx-node-transform n)) t)))) 1 2))

                    ;(printf "S:~a\n" s)
                    (/ 1 s))
                  (start-anim 'bob 'fx/bob-still)
                  (set-gfx-node-scale! (car (gfx-node-children n)) u)
                  (set-gfx-node-translation! current-scene
                                             (lambda (t)
                                               (vec+ (vec* -1 ((lin-tr (gfx-node-transform (thing-node (thing-by-name 'bob)))) t))
                                                     (vec 650 250))))
                  ;(define g1 (build-cmap-from-image 'fx/ch1/well/wellwell-cm 15))
                  ;(define g2 (build-cmap-from-image 'fx/ch1/well/ferme-cm 15))
                  ;(set! current-cmaps (list g1 g2))
                  ;(set! main-r-grid g1)
                  ;(draw-debug-grid 'pf-g1 (hash-keys (make-pf-graph 0 0 (viewport-w) (viewport-h) (combine-cmap g1 g2))))
                  ;(draw-debug-grid 'pfg2 (set->list g1))
                  ;(set! main-grid (make-grid (hash-keys main-r-grid)))
                 ))
                  
(provide thing-by-name load-board propagate-event exec-timers-before)
