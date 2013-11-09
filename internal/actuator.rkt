#lang r5rs
(#%require (only racket/base error))

(#%require "element.rkt")
(#%require "instruction.rkt")
(#%provide Actuator)
(#%provide new-actuator)

;Actuator gives an Instruction which sets its ElementType to a given value (ex: set temperature to 20°C)
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
    ;gives Instruction "set ElementType to a given value"
    ;@param value : the given value
    ;@return Instruction : the corresponding Instruction
    (define (set-value value) (new-instruction-put ((super) 'get-type) value))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((super) (super))
        ((set-value) (apply set-value args))
        (else (error "Error : Actuator.class : unknown method : " message))))
    
    dispatch))
