#lang racket

(require "assets.rkt" "lvl.rkt" "math.rkt")

(displayln "go !")
(define sqw 80)

; I is image-name
(define (build-grid-from-image I)
  (define i (image-by-name I))
  (define (check-square x y)
    (for*/and ([dx (in-range 0 sqw)]
               [dy (in-range 0 sqw)])
      (zero? (car (get-pixel I (vec (+ x dx) (+ y dy)))))))
  (define _ 0)
  (define pos-hash (make-hash))
  (for* ([y (in-range 1 (- (image-h I) sqw) sqw)]
         [x (in-range 0 (- (image-w I) 1) sqw)]
         #:when (check-square x y))
    (hash-set! pos-hash (vec x y) (cons +inf.0 #f)))
  pos-hash)
(define (grid-nearest g u)
  (argmin (lambda (v) (norm (vec- u v))) (hash-keys g)))



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
(provide build-grid-from-image grid-nearest djs)
;(define st (make-heap ()))