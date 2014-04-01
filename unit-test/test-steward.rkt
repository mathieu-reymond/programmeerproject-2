#lang r5rs
(#%require rackunit)

(#%require "../internal/steward.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../physical/physical-room.rkt")
(#%require "../physical/hardware-device.rkt")
(#%provide test-steward)

(define room (new-physical-room "room"))

(define light-device (new-device "light-device" "light-serial-number"))
(define light-sensor (new-sensor LIGHT))
(define light-actuator (new-actuator LIGHT))
(light-device 'add-element light-sensor)
(light-device 'add-element light-actuator)
(new-hardware-device (light-device 'get-serial-number) room)

(define temp-device (new-device "temp-device" "temp-serial-number"))
(define temp-sensor (new-sensor TEMPERATURE))
(define temp-actuator (new-actuator TEMPERATURE))
(temp-device 'add-element temp-sensor)
(temp-device 'add-element temp-actuator)
(new-hardware-device (temp-device 'get-serial-number) room)

(define s (new-steward "bedroom"))

(define test-steward (lambda () (test-case
                                "TEST:steward.rkt"
                                (check-equal? (s 'class) Steward "method(class)")
                                (check-equal? (s 'get LIGHT) #f "method(get); no device")
                                (check-equal? (s 'set LIGHT 1) #f "method(set); no device")
                                (s 'add-device light-device)
                                (check-equal? (s 'get TEMPERATURE) #f "method(get); wrong device")
                                (check-equal? (s 'set TEMPERATURE 21) #f "method(set); wrong device")
                                (s 'add-device temp-device)
                                (check-equal? (s 'set LIGHT 0) #t "method(set); with appropriate device")
                                (check-equal? (s 'get LIGHT) 0 "method(get); with appropriate device")
                                (s 'remove-device temp-device)
                                (check-equal? (s 'get-devices) (list light-device) "method(remove-device)")
                                (s 'remove-device light-device)
                                (check-equal? (s 'get-devices) '() "method(remove-device)")
                                )))
