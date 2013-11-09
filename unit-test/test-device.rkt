#lang r5rs
(#%require rackunit)

(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-device)

(define d (new-device "my-device" "my-serial-number"))
(define light-sensor (new-sensor LIGHT))
(define light-actuator (new-actuator LIGHT))

(define test-device (lambda () (test-case
                                "TEST:device.rkt"
                                (check-equal? (d 'class) Device "method(class)")
                                (check-equal? (d 'get-name) "my-device" "method(get-name)")
                                (check-equal? (d 'get-serial-number) "my-serial-number" "method(get-serial-number)")
                                (check-equal? (d 'get LIGHT) #f "method(get); no sensor")
                                (d 'add-element light-sensor)
                                (check-equal? (d 'get TEMPERATURE) #f "method(get); wrong sensor")
                                (fail (d 'get LIGHT) "method(get); with appropriate sensor") ;TODO
                                (check-equal? (d 'set LIGHT 1) #f "method(set); no actuator")
                                (d 'add-element light-actuator)
                                (check-equal? (d 'set TEMPERATURE 21) #f "method(set); wrong actuator")
                                (fail (d 'set LIGHT 1) "method(set); with appropriate actuator") ;TODO
                                )))
