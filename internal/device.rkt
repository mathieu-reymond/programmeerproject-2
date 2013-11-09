#lang r5rs
(#%require (only racket/base error))

(#%require "actuator.rkt")
(#%require "sensor.rkt")
(#%provide Device)
(#%provide new-device)

;Device holds multiple Sensors and Actuators.
;It can get information and modify its environment using those Sensors and Actuators.

;class name
(define Device 'device)

;constructor
;@param name : this Device's name
;@param serial-number : this Device's serial-number
(define (new-device name serial-number)
  (let ((elements '()))
    ;get class-name
    (define (class) Device)
    ;get name
    (define (get-name) name)
    ;get serial-number
    (define (get-serial-number) serial-number)
    ;get all this Device's Elements
    (define (get-elements) elements)
    ;add an element to the Device
    ;@param element : the Element to add
    (define (add-element element) (set! element (cons element (get-elements))))
    
    (define (get element-type)
      (let loop ((els (get-elements)))
        (if (empty? els)
            #f ;looped over all elements, no matching for request
            (let ((current (car els)))
              (if (and (equal? (current 'class) Sensor)
                       (equal? ((current 'super) 'get-type) element-type))
                  (let ((is (current 'get-value)))
                    ;send instruction-set to physical device using messenger:send
                    )
                  (loop (cdr els)))))))
    
    (define (set element-type value)
      (let loop ((els (get-elements)))
        (if (empty? els)
            #f ;looped over all elements, no matching for request
            (let ((current (car els)))
              (if (and (equal? (current 'class) Actuator)
                       (equal? ((current 'super) 'get-type) element-type))
                  (let ((is (current 'set-value value)))
                    ;send instruction-set to physical device using messenger:send
                    )
                  (loop (cdr els)))))))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((get-name) (get-name))
        ((get-serial-number) (get-serial-number))
        ;((get-elements) (get-elements)) should be intern method
        ((add-element) (apply add-element args))
        ((get) (apply get args))
        ((set) (apply set args))
        (else (error "Error : Device.class : unknown method : " message))))
    
    dispatch))
