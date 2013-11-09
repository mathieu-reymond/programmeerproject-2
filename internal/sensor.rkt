#lang r5rs
(#%require (only racket/base error))

(#%require "element.rkt")
(#%require "instruction.rkt")
(#%provide Sensor)
(#%provide new-sensor)

;Sensor gives an Instruction "gets information (according to it's ElementType)"
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
    ;gives Instruction "get the value of ElementType" 
    ;@return Instruction : the corresponding Instruction
    (define (get-value) (new-instruction-get ((super) 'get-type)))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((super) (super))
        ((get-value) (get-value))
        (else (error "Error : Sensor.class : unknown method : " message))))
    
    dispatch))
