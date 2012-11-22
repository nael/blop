#lang racket/gui

(define (repl-type text-field evt)
  (when (equal? (send evt get-event-type) 'text-field-enter)
    (define ed (send text-field get-editor))
    (define text (send ed get-text))
    (printf "eval : ~a\n" text)
    (eval (call-with-input-string text read))
    (send ed erase)))
(define (make-repl)
  (define win (new frame% [label "REPL"]))
  (new text-field% [label "type !"] [parent win] [callback repl-type])
  (send win show #t))

(provide make-repl)
