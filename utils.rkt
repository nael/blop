#lang racket

; from Greg Hendershott on racket-users
(define (stack-trace-string exn) 
  (string-join 
   (map (lambda (x) 
          (format "\t'~a' ~a:~a" 
                  (if (car x) (car x) "") 
                  (if (cdr x) (srcloc-source (cdr x)) "") 
                  (if (cdr x) (srcloc-line (cdr x)) ""))) 
        (continuation-mark-set->context (exn-continuation-marks exn))) 
   "\n"))
(define (default-exn-handler e)
  (printf "Uncaught exception : ~a\n" e)
  (when (exn? e)
    (printf (stack-trace-string e))
    (printf "\n\n")))
(define counters (make-hash)); again, possible racecond if multithreaded and dosent support nesting for the same counter
(define (with-counter name th)
  (define t0 (current-inexact-milliseconds))
  (define res (th))
  (define el (- (current-inexact-milliseconds) t0))
  (hash-update! counters name (lambda (old) (+ old el)) 0)
  res)
(define (print-counters)
  (for ([(c v) counters])
    (printf "~a: ~a ms\n" c v))
  (set! counters (make-hash)))
(provide default-exn-handler with-counter print-counters)