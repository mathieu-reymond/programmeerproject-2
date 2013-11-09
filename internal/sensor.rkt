#lang r5rs
(#%require (only racket/base error))

(#%require "element.rkt")
(#%require "instruction-set.rkt")
(#%provide Sensor)
(#%provide new-sensor)

;Sensor gets information (according to it's ElementType) about its environment.
;Superclass : Element

;class name
(define Sensor 'sensor)

;constructor
;@param element-type : the type of information this sensor will give
(define (new-sensor element-type)
  (let ((element (new-element element-type)))
    ;get class-name
    (define (class) Sensor)
    ;get superclass
    (define (super) element)
    ;get the value of ElementType in this Sensor's environment
    (define (get-value) (instruction-get ((super) 'get-type)))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((super) (super))
        ((get-value) (get-value))
        (else (error "Error : Sensor.class : unknown method : " message))))
    
    dispatch))
