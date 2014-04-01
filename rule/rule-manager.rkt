#lang r5rs
(#%provide new-rule-manager)
(#%require (only racket/base sleep))
(#%require (only racket/base thread))

(define sleep-time 1)

(define (new-rule-manager steward)
  (let ((rules '()))
    (define (get-steward) steward)
    (define (add-rule rule) (set! rules (cons rule rules)))
    (define (get-rules) rules)
    (define (manage)
      (define (refresh)
        (for-each (lambda (r) (r 'execute steward)) rules)
        (sleep sleep-time)
        (refresh))
      (thread refresh))
    
    (define (dispatch message . args)
      (case message
        ((get-steward) (get-steward))
        ((get-rules) (get-rules))
        ((add-rule) (apply add-rule args))
        ((manage) (manage))))
    
    dispatch))