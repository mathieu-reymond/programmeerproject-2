#lang r5rs
(#%require (only racket/base error))

(#%require "element-type.rkt")
(#%provide Element)
(#%provide new-element)

;Element is an abstract class.
;It holds an ElementType.
;Implementations : Sensor, Actuator

;class name
(define Element 'element)

;constructor
;@param : element-type : The type this Element holds
(define (new-element element-type)
  ;get class-name
  (define (class) Element)
  ;get this Element's ElementType
  (define (get-type) element-type)
  
  (define (dispatch message . args)
    (case message
      ((class) (class))
      ((get-type) (get-type))
      (else (error "Error : Element.class : unknown method : " message))))
  
  dispatch)