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
    (define (remove-rule rule)
      (define (loop current previous)
        (cond
          ((eq? '() current) #f) ;rule not in list
          ((eq? rule (car current))
           (if (eq? '() previous)
               (set! rules (cdr current))
               (set-cdr! previous (cdr current)))
           #t) ;found and removed rule
          (else (loop (cdr current) current))))
      (loop rules '()))
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
        ((remove-rule) (apply remove-rule args))
        ((manage) (manage))))
    
    dispatch))