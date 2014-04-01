#lang r5rs
(#%require (only racket/base error))

(#%require "steward.rkt")
(#%require "../structure/map.rkt")
(#%require "../communication/action.rkt")
(#%provide CentralUnit)
(#%provide new-central-unit)

(define CentralUnit 'central-unit)

(define (new-central-unit)
  (let ((stewards (new-map)))
    (define (class) CentralUnit)
    (define (get-stewards)
      (stewards 'get-elements))
    (define (for-each-steward proc)
      (for-each proc (get-stewards)))
    (define (add-steward steward)
      (stewards 'add! (steward 'get-room) steward)
      ((new-action (string-append "Added new Steward \""
                                 (steward 'get-room)
                                 "\" to Domotica")) 'write))
    (define (get-steward room)
      (stewards 'find room))
    (define (remove-steward steward)
      (stewards 'remove! (steward 'get-room))
      ((new-action (string-append "Removed Steward \""
                                 (steward 'get-room)
                                 "\" from Domotica")) 'write))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((for-each-steward) (apply for-each-steward args))
        ((add-steward) (apply add-steward args))
        ((get-steward) (apply get-steward args))
        ((get-stewards) (get-stewards))
        ((remove-steward) (apply remove-steward args))
        (else (error "Error : CentralUnit.class : unknown method : " message))))
    
    dispatch))
