#lang r5rs
(#%require (only racket/base error))

(#%require "element.rkt")
(#%require "instruction.rkt")
(#%provide Sensor)
(#%provide new-sensor)

;b===c* internal/sensor
; NAME
;  sensor
; DESCRIPTION
;  Vraagt informatie over zijn element-type op. 
;  Een sensor genereert een instruction die de waarde van zijn element-type opvraagt. 
;  Deze instruction kan daarna via een messenger verzonden worden naar hardware 
;  zodat die het element-type meet.
; PARENTS
;  * element
;e===

;Sensor gives an Instruction "gets information (according to it's ElementType)"
;Superclass : Element

;class name
(define Sensor 'sensor)

;constructor
;@param element-type : the type of information this sensor will give
;b===o* sensor/new-sensor
; NAME
;  new-sensor
; DESCRIPTION
;  Maakt een nieuwe sensor aan.
; PARAMETERS
;  * element-type - het element-type van de sensor.
; SYNOPSIS
(define (new-sensor element-type)
;e===
  (let ((element (new-element element-type)))
    ;get class-name
    ;b===m* sensor/class
    ; NAME
    ;  class
    ; DESCRIPTION
    ;  Geeft de classe terug van dit object.
    ; RETURN VALUE
    ;  symbol - de naam van de classe
    ; SYNOPSIS
    (define (class)
    ; SOURCE
      Sensor)
    ;e===
    ;get superclass
    ;b===m* sensor/super
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
    ;gives Instruction "get the value of ElementType" 
    ;@return Instruction : the corresponding Instruction
    ;b===m* sensor/get-value
    ; NAME
    ;  get-value
    ; DESCRIPTION
    ;  Geeft een instruction-get terug die de waarde van deze sensor's element-type
    ;  kan opvragen.
    ; RETURN VALUE
    ;  instruction-get - de gevraagde instructie.
    ; SYNOPSIS
    (define (get-value)
    ; SOURCE
      (new-instruction-get ((super) 'get-type)))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((super) (super))
        ((get-value) (get-value))
        (else (error "Error : Sensor.class : unknown method : " message))))
    
    dispatch))
