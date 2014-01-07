#lang r5rs
(#%require (only racket/base error))
(#%require (only racket/list empty?))

(#%require "device.rkt")
(#%provide Steward)
(#%provide new-steward)

;Steward is located in a specific room and has multiple Devices.

;class name
(define Steward 'steward)

;constructor
;@param room : the room in which this Steward is located
(define (new-steward room)
  (let ((devices '()))
    ;get class-name
    (define (class) Steward)
    ;get room
    (define (get-room) room)
    ;get devices
    (define (get-devices) devices)
    ;add a device to this steward
    (define (add-device device) (set! devices (cons device devices)))
    
    ;need high-order method (instruction 'instruction . args) ?
    ;ex (instruction 'get element-type)
    (define (get element-type)
      (let loop ((devs (get-devices)))
        (if (empty? devs)
            #f ;no device with elements to match request
            (let ((result ((car devs) 'get element-type)))
              (if result ;result != false means we found a valid result
                  result
                  (loop (cdr devs)))))))
    ;need high-order method (instruction 'instruction . args) ?
    ;ex (instruction 'set element-type value)
    (define (set element-type value)
      (let loop ((devs (get-devices)))
        (if (empty? devs)
            #f ;no device with elements to match request
            (let ((result ((car devs) 'set element-type value)))
              (if result ;result != false means we found a valid result
                  result
                  (loop (cdr devs)))))))
                  
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((get-room) (get-room))
        ((add-device) (apply add-device args))
        ((get) (apply get args))
        ((set) (apply set args))
        ((get-devices) (get-devices)) ;read-only
        (else (error "Error : Steward.class : unknown method : " message))))
    
    dispatch))