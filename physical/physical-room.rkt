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


;Simulates a room.
;A room has a temperature and light-instensity.
;It is possible to get and modify information about a room using hardware-devices
;@param name : The room's name
(define (new-physical-room name)
  (let ((attributes (default-room-attributes)))
    ;get this room's name
    (define (get-name) name)
    ;get the attribute corresponding to element-type
    ;@param element-type : the type of attribute you want to get
    (define (get element-type) (vector-ref attributes (element-type-to-index element-type)))
    ;set the attribute to a new value
    ;@param element-type : the type of attribute you want to set
    ;@param value : the value you want the attribute to be set
    (define (set element-type value) (vector-set! attributes (element-type-to-index element-type) value))
    
    (define (dispatch message . args)
      (case message
        ((get-name) (get-name))
        ((get) (apply get args))
        ((set) (apply set args))
        (else (error "Error : PhysicalRoom.class : unknown method : " message))))
    
    dispatch))
