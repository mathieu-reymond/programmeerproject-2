#lang r5rs
(#%require rackunit)

(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../physical/physical-room.rkt")
(#%require "../physical/hardware-device.rkt")
(#%require "../communication/parser.rkt"
           "../internal/instruction.rkt")
(#%provide test-device)

(define d (new-device "my-device" "my-serial-number"))
(define light-sensor (new-sensor LIGHT))
(define light-actuator (new-actuator LIGHT))

(define room (new-physical-room "room"))
(define hd (new-hardware-device (d 'get-serial-number) room))

(define test-device (lambda () (test-case
                                "TEST:device.rkt"
                                (check-equal? (d 'class) Device "method(class)")
                                (check-equal? (d 'get-name) "my-device" "method(get-name)")
                                (check-equal? (d 'get-serial-number) "my-serial-number" "method(get-serial-number)")
                                (check-equal? (d 'get LIGHT) #f "method(get); no sensor")
                                (d 'add-element light-sensor)
                                (check-equal? (d 'get TEMPERATURE) #f "method(get); wrong sensor")
                                (check-equal? (instruction-to-list (d 'get LIGHT)) 
                                              (instruction-to-list (new-instruction-get LIGHT)) 
                                              "method(get); with appropriate sensor")
                                (check-equal? (d 'set LIGHT 1) #f "method(set); no actuator")
                                (d 'add-element light-actuator)
                                (check-equal? (d 'set TEMPERATURE 21) #f "method(set); wrong actuator")
                                (check-equal? (instruction-to-list (d 'set LIGHT 1))
                                              (instruction-to-list (new-instruction-put LIGHT 1))
                                              "method(set); with appropriate actuator")
                                (check-equal? (device-types 'contains? "my-device") #t "method(contains?)")
                                (check-equal? (device-types 'contains? "another-device") #f "method(contains?)")
                                (check-equal? (d 'get-elements) ((new-device "my-device" "another-serial-number") 'get-elements) "initializing") 
                                )))
