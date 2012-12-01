#lang racket

(require racket/gui "assets.rkt" "lvl.rkt" "math.rkt")

(define sqw 20)

; a collision map is a set of points which are obstacles for at most a sqw-sided square
(struct cmap (points sqw min max image))

; the pf graph is a hash map posn -> vertex state

; I is image-name
(define (build-cmap-from-image I)
  (define cmw (ceiling (/ (image-w I) sqw)))
  (define cmh (ceiling (/ (image-h I) sqw)))
  (define cmap (make-object bitmap% cmw cmh #f #t))
  (for* ([x (in-range 0 cmw)]
        [y (in-range 0 cmh)])
    ;(when (rect-transparent? I (* x sqw) (* y sqw) sqw sqw)
    ;  (printf "OK ~a ~a (~ax~a)\n" x y cmw cmh)
    ;  (send cmap set-argb-pixels x y 1 1 #"\0\255\255\255"))
    (unless (rect-transparent? I (* x sqw) (* y sqw) sqw sqw)
      (send cmap set-argb-pixels x (- cmh y 1) 1 1 #"\255\0\0\0")))
  (define cmapsym (gensym (format "~a-downscaled" (symbol->string I))))
  (register-dynamic-image cmapsym cmap (lin-compose (texture-transform I) (lin-hom sqw)))
  cmapsym)
  ;(transform-cmap cm (texture-transform I)))
(define (make-pf-graph x0 y0 w h cmaps)
  (define gc (make-hash))
  (for* ([y (in-range (+ x0 (/ sqw 2)) (- h (/ sqw 2)) sqw)]
         [x (in-range (+ y0 (/ sqw 2)) (- w (/ sqw 2) 1) sqw)])
    (unless (for/or ([cm cmaps]) (cmap-collision? (cdr cm) (lin-apply (lin-inverse (car cm)) (vec x y))))
      (hash-set! gc (vec x y) (cons +inf.0 #f))))
  gc)
(define (combine-cmap c1 c2)
  (cmap (set-union (cmap-points c1) (cmap-points c2))
        (max (cmap-sqw c1) (cmap-sqw c2))
        (vec (min (vec-x (cmap-min c1)) (vec-x (cmap-min c2)))
             (min (vec-y (cmap-min c1)) (vec-y (cmap-min c2))))
        (vec (max (vec-x (cmap-max c1)) (vec-x (cmap-max c2)))
             (max (vec-y (cmap-max c1)) (vec-y (cmap-max c2))))
        #f))
(define (grid-nearest g u)
  (argmin (lambda (v) (norm (vec- u v))) (hash-keys g)))
(define (cmap-collision? cm u0)
  (define u (lin-apply (lin-inverse (texture-transform cm)) u0))
  (if (and (<= 0 (vec-x u))
           (<= 0 (vec-y u))
           (> (image-w cm) (vec-x u))
           (> (image-h cm) (vec-y u)))
      (not (pixel-transparent? cm (vec->int-vec u)))
      #f))


(define (insert-sorted cmp lst n)
  (cond
    [(or (empty? lst) (cmp n (first lst))) (cons n lst)]
    [else (cons (first lst) (insert-sorted cmp (rest lst) n))]))


(define (djs pos-hash f t)
  (hash-set! pos-hash f (cons 0 f))
  (define Q (list f)) ; todo inefficient, should use a heap
  
  (define (h u)
    (norm (vec- u t)))
  (define (ds u v)
    (+ (norm (vec- u v))
       (- (h u))
       (h v)))
  (define (dist n)
    (car (hash-ref pos-hash n)))
  (define (nhs u)
    (define ds (list (vec sqw 0)
                   (vec 0 sqw)
                   (vec (- sqw) 0)
                   (vec 0 (- sqw))))
    (filter ((curry hash-has-key?) pos-hash) (map (lambda (d) (vec+ d u)) ds)))
  (define (run-djs)
    (if (null? Q)
        #f
        (let ([hm (car Q)])
          (cond
            [(equal? hm t) (backward-prop t null)]
            [(equal? (dist hm) +inf.0) (displayln "Search exhausted") #f]
            [else (set! Q (cdr Q))
                  (update hm)
                  (run-djs)]))))
  
  (define (update n)
    (for ([nh (nhs n)])
      (define a (+ (dist n) (ds n nh)))
      (when (< a (dist nh))
        (hash-set! pos-hash nh (cons a n))
        (set! Q (insert-sorted (lambda (u v) (<= (dist u) (dist v))) Q nh)))))
  (define (backward-prop n acc)
    (define p (cdr (hash-ref pos-hash n)))
    (if (equal? p n)
        acc
        (backward-prop p (cons n acc))))
  
  (define path (run-djs))
  
  (for ([(k _) pos-hash]) (hash-set! pos-hash k (cons +inf.0 #f))) ; reset state
  
  path)
;(displayln pos-hash)
;(define f (car (hash-keys pos-hash)))
;(djs f (vec+ f (vec sqw (* 2 sqw))))
(provide (struct-out cmap) build-cmap-from-image grid-nearest djs combine-cmap make-pf-graph cmap-collision?)
;(define st (make-heap ()))