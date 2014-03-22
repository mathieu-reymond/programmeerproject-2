#lang racket
(provide new-rule-manager)

(define sleep-time 1)

(define (new-rule-manager steward)
  (let ((rules '()))
    (define (add-rule rule) (set! rules (cons rule rules)))
    (define (manage)
      (define (refresh)
        (for-each (lambda (r) (r 'execute steward)) rules)
        (sleep sleep-time)
        (refresh))
      (thread refresh))
    
    (define (dispatch message . args)
      (case message
        ((add-rule) (apply add-rule args))
        ((manage) (manage))))
    
    dispatch))