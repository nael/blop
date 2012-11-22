#lang racket

(require "assets.rkt" "lvl.rkt" "math.rkt")
(define I 'fx/ch1/well/wellwell)
(define i (image-by-name I))
(displayln "go !")
(define sqw 30)

(define (check-square x y)
  (for*/and ([dx (in-range 0 sqw)]
            [dy (in-range 0 sqw)])
    (zero? (car (get-pixel I (vec (+ x dx) (+ y dy)))))))
(define _ 0)
(define pos-hash (make-hash))
(for* ([y (in-range 1 (- (image-h I) sqw) sqw)]
       [x (in-range 0 (- (image-w I) 1) sqw)]
       #:when (check-square x y))
  (hash-set! pos-hash (vec x y) (cons +inf.0 #f))
  ;    (when (>= (*  _ sqw) (image-w I)) (printf "\n") (set! _ 0))
  ;    (set! _ (add1 _));
  ;    (if (check-square x y)
  ;        (begin (printf ".") 1)
  ;        (printf "0"))))
  )

(define (dist n)
  (car (hash-ref pos-hash n)))

(define (insert-sorted cmp lst n)
  (cond
    [(or (empty? lst) (cmp n (first lst))) (cons n lst)]
    [else (cons (first lst) (insert-sorted cmp (rest lst) n))]))
(define (nhs u)
  (define ds (list (vec sqw 0)
                   (vec 0 sqw)
                   (vec (- sqw) 0)
                   (vec 0 (- sqw))))
  (filter ((curry hash-has-key?) pos-hash) (map (lambda (d) (vec+ d u)) ds)))
(define (ds u v)
  (norm (vec- u v)))
(define (djs f t)
  (hash-set! pos-hash f (cons 0 f))
  (define Q (list f)) ; todo inefficient, should use a heap
  
  
  (define (run-djs)
    (if (null? Q)
        (displayln "Search exhausted\n");(backward-prop t)
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
  
  (run-djs))
;(displayln pos-hash)
(define f (car (hash-keys pos-hash)))
(djs f (vec+ f (vec sqw (* 2 sqw))))
;(define st (make-heap ()))