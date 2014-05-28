#lang r5rs
(#%require (only racket/base error))
(#%require (only racket/base unquote))

(#%require "../internal/element-type.rkt")
(#%provide new-physical-room)

;index of a given attribute in attribute-vector of room
(define TEMPERATURE_IDX 0)
(define LIGHT_IDX 1)

;given an element-type, returns the corresponding index
(define (element-type-to-index element-type)
  (cond
    ((equal? element-type TEMPERATURE) TEMPERATURE_IDX)
    ((equal? element-type LIGHT) LIGHT_IDX)
    (else (error "Error : element-type-to-index : unknown element-type : " element-type))))

;initialize room attributes
;@return vector : the vector with corresponding attributes
(define (default-room-attributes)
  ;TEMPERATURE_IDX : default temperature in °C
  ;LIGHT_IDX : default ligth-intensity (0..1)
  (vector 20 1))

;b===c* physical/physical-room
; NAME
;  physical-room
; DESCRIPTION
;  Simuleert een kamer. Een kamer heeft verschillende element-type attributen zoals LIGHT.
;  Deze klasse wordt gebruikt voor het simuleren van xbee toestellen (via xbee-simulation).
;  Een zigbee-message wordt afgehandelt door execute-zigbee-instruction,
;  die de attributen van physical-room gaat opvragen en aanpassen.
;e===
;b===o* physical-room/new-physical-room
; NAME
;  new-physical-room
; DESCRIPTION
;  Maakt een nieuwe physical-room aan.
; PARAMETERS
;  * de naam van de physical-room.
; SYNOPSIS
(define (new-physical-room name)
;e===
  (let ((attributes (default-room-attributes)))
    ;b===m* physical-room/get-name
    ; NAME
    ;  get-name
    ; DESCRIPTION
    ;  Geeft de naam van deze room terug.
    ; RETURN VALUE
    ;  string - de naam van deze room.
    ; SYNOPSIS
    (define (get-name)
    ; SOURCE
      name)
    ;e===
    ;b===m* physical-room/get
    ; NAME
    ;  get
    ; DESCRIPTION
    ;  Geeft de waarde van een element-type van deze physical-room terug.
    ; PARAMETERS
    ;  * element-type - de element-type waarvan de waarde gevraagd is.
    ; RETURN VALUE
    ;  integer - de waarde van het element-type
    ; SYNOPSIS
    (define (get element-type)
    ; SOURCE
      (vector-ref attributes (element-type-to-index element-type)))
    ;e===
    ;b===m* physical-room/set
    ; NAME
    ;  set
    ; DESCRIPTION
    ;  Past de waarde van een element-type van deze physical-room aan.
    ; PARAMETERS
    ;  * element-type - de element-type waarvan de waarde aangepast moet worden.
    ;  * value - de nieuwe waarde van dit element-type.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (set element-type value)
    ; SOURCE
      (vector-set! attributes (element-type-to-index element-type) value) #t)
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((get-name) (get-name))
        ((get) (apply get args))
        ((set) (apply set args))
        (else (error "Error : PhysicalRoom.class : unknown method : " message))))
    
    dispatch))
