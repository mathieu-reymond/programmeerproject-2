#lang r5rs
(#%require  "../structure/map.rkt")
(#%provide TEMPERATURE
           LIGHT
           element-type-zigbee-type-map
           for-each-element-type
           to-string)

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

(define element-type-zigbee-type-map (new-map))
(element-type-zigbee-type-map 'add! TEMPERATURE "TEM")
(element-type-zigbee-type-map 'add! LIGHT "POW")