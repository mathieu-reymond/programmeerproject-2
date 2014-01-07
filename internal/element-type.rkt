#lang r5rs

(#%provide TEMPERATURE)
(#%provide LIGHT)
(#%provide for-each-element-type)
(#%provide to-string)

;ElementType is an Enum Class representing all different types the element can have (temperature, light...)

(define TEMPERATURE 0)
(define LIGHT 1)

(define (for-each-element-type proc)
  (for-each proc (list TEMPERATURE
                       LIGHT)))

(define (to-string element-type)
  (cond
    ((= element-type TEMPERATURE) "Temperature")
    ((= element-type LIGHT) "Light")))