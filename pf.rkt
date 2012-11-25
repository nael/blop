#lang racket

(require "assets.rkt" "lvl.rkt" "math.rkt")

(displayln "go !")
(define sqw 40)

; a collision map is a set of points which are obstacles for at most a sqw-sided square
; the pf graph is a hash map posn -> vertex state

; I is image-name
(define (build-cmap-from-image I)
  (define i (image-by-name I))
  (define (check-square x y)
    (for*/and ([dx (in-range (- (/ sqw 2)) (/ sqw 2))]
               [dy (in-range (- (/ sqw 2)) (/ sqw 2))])
      (zero? (car (get-pixel I (vec (+ x dx) (+ y dy)))))))
  (define _ 0)
  (define pos-hash (make-hash))
  (for* ([y (in-range (- (* 2 sqw)) (+ (image-h I) (* 2 sqw)) sqw)]
         [x (in-range (- (* 2 sqw)) (+ (image-w I) (* 2 sqw) 1) sqw)]
         #:when (check-square x y))
    (hash-set! pos-hash (vec x y) (cons +inf.0 #f)))
  (transform-grid pos-hash (texture-transform I)))
(define (combine-grids x0 y0 w h g1 g2)
  (define gc (make-hash))
  (for* ([y (in-range (+ x0 (/ sqw 2)) (- h (/ sqw 2)) sqw)]
         [x (in-range (+ y0 (/ sqw 2)) (- w (/ sqw 2) 1) sqw)])
    (define (ok? g)
      (<= (norm (vec- (vec x y) (grid-nearest g (vec x y)))) (/ sqw 2)))
    (when (and (ok? g1) (ok? g2))
      (hash-set! gc (vec x y) (cons +inf.0 #f))))
  gc)
(define (grid-nearest g u)
  (argmin (lambda (v) (norm (vec- u v))) (hash-keys g)))
(define (transform-grid g f)
  (define res (make-hash))
  (hash-for-each g
                 (lambda (k v)
                   (hash-set! res (lin-apply f k) v)))
  res)


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
(provide build-grid-from-image grid-nearest djs combine-grids)
;(define st (make-heap ()))