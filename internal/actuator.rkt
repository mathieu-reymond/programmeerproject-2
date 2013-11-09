#lang r5rs
(#%require (only racket/base error))

(#%require "element.rkt")
(#%require "instruction-set.rkt")
(#%provide Actuator)
(#%provide new-actuator)

;Actuator set its ElementType (in its environment) to a given value (ex: set temperature to 20°C)
;Superclass : Element

;class name
(define Actuator 'actuator)

;constructor
;@param element-type : the type that will be modified
(define (new-actuator element-type)
  (let ((element (new-element element-type)))
    ;get class-name
    (define (class) Actuator)
    ;get superclass
    (define (super) element)
    ;set environment's ElementType to a given value
    ;@param value : the given value
    (define (set-value value) (instruction-put (super 'get-type) value))
    
    (define (dipsatch message .args)
      (case message
        ((class) (class))
        ((super) (super))
        ((set-value) (apply set-value args))
        (else (error "Error : Actuator.class : unknown method : " message))))
    
    dispatch))
