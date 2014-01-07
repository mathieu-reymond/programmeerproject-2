#lang r5rs
(#%require (only racket/base error))

(#%require "steward.rkt")
(#%provide CentralUnit)
(#%provide new-central-unit)

(define CentralUnit 'central-unit)

(define (new-central-unit)
  (let ((stewards '()))
    (define (class) CentralUnit)
    (define (for-each-steward proc)
      (for-each proc stewards))
    (define (add-steward steward)
      (set! stewards (cons steward stewards)))
    (define (get-steward room)
      (let ((steward #f))
        (for-each-steward (lambda(s) (if (equal? (s 'get-room) room) (set! steward s))))
        steward))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((for-each-steward) (apply for-each-steward args))
        ((add-steward) (apply add-steward args))
        ((get-steward) (apply get-steward args))
        (else (error "Error : CentralUnit.class : unknown method : " message))))
    
    dispatch))
