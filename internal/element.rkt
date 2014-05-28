#lang r5rs
(#%require (only racket/base error))

(#%require "element-type.rkt")
(#%provide Element)
(#%provide new-element)

;b===c* internal/element
; NAME
;  element
; DESCRIPTION
;  Een abstracte classe die een element-type bijhoudt.
; CHILDREN
;  * sensor
;  * actuator
;e===

;Element is an abstract class.
;It holds an ElementType.
;Implementations : Sensor, Actuator

;class name
(define Element 'element)

;constructor
;@param : element-type : The type this Element holds
;b===o* element/new-element
; NAME
;  new-element
; DESCRIPTION
;  Maakt een nieuw element object.
; PARAMETERS
;  * element-type - het element-type die dit element representeert
; SYNOPSIS
(define (new-element element-type)
;e===
  ;b===m* element/class
  ; NAME
  ;  class
  ; DESCRIPTION
  ;  Geeft de classe terug van dit object.
  ; RETURN VALUE
  ;  symbol - de naam van de classe
  ; SYNOPSIS
  (define (class)
  ; SOURCE
    Element)
  ;e===
  ;get this Element's ElementType
  ;b===m* element/get-type
  ; NAME
  ;  get-type
  ; DESCRIPTION
  ;  Geeft het element-type van dit element terug.
  ; RETURN VALUE
  ;  element-type - het object's element-type
  ; SYNOPSYS
  (define (get-type)
  ; SOURCE
    element-type)
  ;e===
  
  (define (dispatch message . args)
    (case message
      ((class) (class))
      ((get-type) (get-type))
      (else (error "Error : Element.class : unknown method : " message))))
  
  dispatch)