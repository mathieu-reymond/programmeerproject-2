#lang r5rs
(#%require  "../structure/map.rkt")
(#%provide TEMPERATURE
           LIGHT
           element-type-zigbee-type-map
           for-each-element-type
           to-string)
;ElementType is an Enum Class representing all different types the element can have (temperature, light...)

;b===c* internal/element-type
; NAME
;  element-type
; DESCRIPTION
;  Deze classe stelt alle verschillende types elementen (zoals temperatuur) die
;  door het domotica systeem gebruikt kunnen worden. Andere element-types die de verschillende
;  sensoren kunnen meten zullen door het domotica-systeem genegeerd worden.
;e===

;b===v* element-type/TEMPERATURE
; NAME
;  TEMPERATURE
; DESCRIPTION
;  Stelt de temperatuur voor.
; SOURCE
(define TEMPERATURE 0)
;e===
;b===v* element-type/LIGHT
; NAME
;  LIGHT
; DESCRIPTION
;  Stelt het licht voor.
; SOURCE
(define LIGHT 1)
;e===

;b===m* element-type/for-each-element-type
; NAME
;  for-each-element-type
; DESCRIPTION
;  Deze methode past een procedure op elk element-type toe.
; PARAMETERS
;  * proc - de procedure die op elk element-type toegepast moet worden.
; RETURN VALUE
;  #<void>
; SYNOPSIS
(define (for-each-element-type proc)
; SOURCE
  (for-each proc (list TEMPERATURE
                       LIGHT)))
;e===

;b===m* element-type/to-string
; NAME
;  to-string
; DESCRIPTION
;  De string-representatie van een element-type
; PARAMETERS
;  * element-type - het type dat omgezet moet worden.
; RETURN VALUE
;  string - de string-representatie van element-type
; SYNOPSIS
(define (to-string element-type)
; SOURCE
  (cond
    ((= element-type TEMPERATURE) "Temperature")
    ((= element-type LIGHT) "Light")))
;e===

;b===v* element-type/element-type-zigbee-type-map
; NAME
;  element-type-zigbee-type-map
; DESCRIPTION
;  Elk element-type gebruikt door het domotica systeem heeft een specifieke representatie
;  in het zigbee protocol. Deze map mapt elk element-type naar zijn zigbee representatie.
; SOURCE
(define element-type-zigbee-type-map (new-map))
(element-type-zigbee-type-map 'add! TEMPERATURE "TEM")
(element-type-zigbee-type-map 'add! LIGHT "POW")
;e===