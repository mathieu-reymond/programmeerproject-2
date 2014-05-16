#lang r5rs
(#%require (only racket/base error)
           (only racket/list empty?))

(#%require "device.rkt"
           "../communication/action.rkt"
           "../communication/messenger.rkt"
           "element-type.rkt"
           "../rule/rule-manager.rkt")
(#%provide Steward)
(#%provide new-steward)

;Steward is located in a specific room and has multiple Devices.

;class name
(define Steward 'steward)

;constructor
;@param room : the room in which this Steward is located
(define (new-steward room)
  (let ((devices '())
        (rule-manager '()))
    ;get class-name
    (define (class) Steward)
    ;get room
    (define (get-room) room)
    ;get devices
    (define (get-devices) devices)
    ;remove a device
    (define (remove-device device)
      (define (loop previous current)
        (cond
          ((eq? '() current) #f) ;not in list
          ((eq? (car current) device)
           (if (eq? '() previous)
               (set! devices (cdr devices))
               (set-cdr! previous (cdr current))))
          (else
           (loop current (cdr current)))))
      (loop '() (get-devices))
      ((new-action (string-append "Removed device \""
                                  (device 'get-name)
                                  "\" from steward \""
                                  (get-room)
                                  "\"")) 'write))
    ;add a device to this steward
    (define (add-device device) 
      (set! devices (cons device devices))
      ((new-action (string-append "Added device \""
                                  (device 'get-name)
                                  "\" to steward \""
                                  (get-room)
                                  "\"")) 'write))
    
    ;need high-order method (instruction 'instruction . args) ?
    ;ex (instruction 'get element-type)
    (define (get element-type)
      (let loop ((devs (get-devices)))
        (if (empty? devs)
            #f ;no device with elements to match request
            (let ((is ((car devs) 'get element-type)))
              (if is ;result != false means we found a valid result
                  ((send room (car devs) is) 'get-value)
                  (loop (cdr devs)))))))
    ;need high-order method (instruction 'instruction . args) ?
    ;ex (instruction 'set element-type value)
    (define (set element-type value)
      (let loop ((devs (get-devices)))
        (if (empty? devs)
            #f ;no device with elements to match request
            (let ((is ((car devs) 'set element-type value)))
              (cond
                (is ;result != false means we found a valid result
                 ((new-action (string-append "Set "
                                             (to-string element-type)
                                             " to "
                                             (number->string value)
                                             " in steward \""
                                             (get-room)
                                             "\"")) 'write)
                 ((send room (car devs) is) 'get-value))
                (else (loop (cdr devs))))))))
    
    (define (get-rule-manager) rule-manager)
    
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((get-room) (get-room))
        ((add-device) (apply add-device args))
        ((remove-device) (apply remove-device args))
        ((get) (apply get args))
        ((set) (apply set args))
        ((get-devices) (get-devices)) ;read-only
        ((get-rule-manager) (get-rule-manager))
        (else (error "Error : Steward.class : unknown method : " message))))
    
    (set! rule-manager (new-rule-manager dispatch))
    ;client ports
    (steward-port-map 'add! (get-room) (new-steward-ports dispatch))
    dispatch))