#lang r5rs
(#%require (only racket/base error))

(#%require "element.rkt")
(#%require "instruction.rkt")
(#%provide Actuator)
(#%provide new-actuator)

;b===c* internal/actuator
; NAME
;  actuator
; DESCRIPTION
;  Laat toe om de informatie van zijn element-type aan te passen. 
;  Een actuator genereert een instruction die later verzonden kan worden naar de hardware.
;  Deze instruction past de waarde van element-type naar een nieuwe waarde toe.
; PARENTS
;  * element
;e===

;Actuator gives an Instruction which sets its ElementType to a given value (ex: set temperature to 20°C)
;Superclass : Element

;class name
(define Actuator 'actuator)

;constructor
;@param element-type : the type that will be modified
;b===o* actuator/new-actuator
; NAME
;  new-actuator
; DESCRIPTION
;  Maakt een nieuwe actuator aan.
; PARAMETERS
;  * element-type - het element-type van de actuator.
; SYNOPSIS
(define (new-actuator element-type)
;e===
  (let ((element (new-element element-type)))
    ;get class-name
    ;b===m* actuator/class
    ; NAME
    ;  class
    ; DESCRIPTION
    ;  Geeft de classe terug van dit object.
    ; RETURN VALUE
    ;  symbol - de naam van de classe
    ; SYNOPSIS
    (define (class)
    ; SOURCE
      Actuator)
    ;e===
    ;get superclass
    ;b===m* actuator/super
    ; NAME
    ;  super
    ; DESCRIPTION
    ;  Geeft de super-classe van dit object terug
    ; RETURN VALUE
    ;  element - de super-classe
    ; SYNOPSIS
    (define (super)
    ; SOURCE
      element)
    ;e===
    ;gives Instruction "set ElementType to a given value"
    ;@param value : the given value
    ;@return Instruction : the corresponding Instruction
    ;b===m* actuator/set-value
    ; NAME
    ;  set-value
    ; DESCRIPTION
    ;  Geeft een instruction-put terug die het element-type van deze actuator aanpast
    ;  op een bepaalde waarde.
    ; PARAMETERS
    ;  * value - De nieuwe waarde.
    ; RETURN VALUE
    ;  instruction-put - de gevraagde instructie.
    ; SYNOPSIS
    (define (set-value value)
    ; SOURCE
      (new-instruction-put ((super) 'get-type) value))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((super) (super))
        ((set-value) (apply set-value args))
        (else (error "Error : Actuator.class : unknown method : " message))))
    
    dispatch))
