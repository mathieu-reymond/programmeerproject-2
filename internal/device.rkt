#lang r5rs
(#%require (only racket/base error))
(#%require (only racket/list empty?))

(#%require "actuator.rkt")
(#%require "sensor.rkt")
(#%require "../communication/messenger.rkt")
(#%require "../communication/action.rkt")
(#%provide Device)
(#%provide new-device)
(#%provide device-types)

;Device holds multiple Sensors and Actuators.
;It can get information and modify its environment using those Sensors and Actuators.

(define (new-device-types)
  (let ((devices '()))
    (define (for-each-device-type proc)
      (for-each proc devices))
    (define (contains? device-name)
      (let ((already-exists #f))
        (for-each-device-type (lambda(d) (set! already-exists (or already-exists (equal? (d 'get-name) device-name)))))
        already-exists))
    (define (add-device device)
      (if (not (contains? (device 'get-name)))
          (begin
            (set! devices (cons device devices))
            ((new-action (string-append "Created device \""
                                        (device 'get-name)
                                        "\"")) 'write))
          #f))
    (define (get-device name)
      (let ((device #f))
        (for-each-device-type (lambda(d) (if (equal? name (d 'get-name)) (set! device d) #f)))
        device))
    
    (define (dispatch message . args)
      (case message
        ((for-each-device-type) (apply for-each-device-type args))
        ((contains?) (apply contains? args))
        ((add-device) (apply add-device args))
        ((get-device) (apply get-device args))))
    dispatch))

(define device-types (new-device-types))


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
    (define (add-element element) (set! elements (cons element (get-elements))))
    
    (define (get element-type)
      (let loop ((els (get-elements)))
        (if (empty? els)
            #f ;looped over all elements, no matching for request
            (let ((current (car els)))
              (if (and (equal? (current 'class) Sensor)
                       (equal? ((current 'super) 'get-type) element-type))
                  (let ((is (current 'get-value)))
                    ;send instruction-set to physical device using messenger:send
                    ((send dispatch is) 'get-value) ;should be a RET-instruction, with response stored in value
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
                    ((send dispatch is) 'get-value) ;should be a RET-instruction, with response stored in value
                    )
                  (loop (cdr els)))))))
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((get-name) (get-name))
        ((get-serial-number) (get-serial-number))
        ((add-element) (apply add-element args))
        ((get) (apply get args))
        ((set) (apply set args))
        ((get-elements) (get-elements)) ;should be intern method
        (else (error "Error : Device.class : unknown method : " message))))
    
    ;initialize
    (if (device-types 'contains? (get-name))
        (let ((device (device-types 'get-device (get-name))))
          (set! elements (device 'get-elements)))
        (device-types 'add-device dispatch))
    
    dispatch))
