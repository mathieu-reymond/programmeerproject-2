#lang r5rs
(#%require rackunit)

(#%require "../internal/steward.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-steward)

(define light-device (new-device "light-device" "light-serial-number"))
(define light-sensor (new-sensor LIGHT))
(define light-actuator (new-actuator LIGHT))
(light-device 'add-element light-sensor)
(light-device 'add-element light-actuator)

(define temp-device (new-device "temp-device" "temp-serial-number"))
(define temp-sensor (new-sensor TEMPERATURE))
(define temp-actuator (new-actuator TEMPERATURE))
(temp-device 'add-element temp-sensor)
(temp-device 'add-element temp-actuator)

(define s (new-steward "bedroom"))

(define test-steward (lambda () (test-case
                                "TEST:steward.rkt"
                                (check-equal? (s 'class) Steward "method(class)")
                                (check-equal? (s 'get LIGHT) #f "method(get); no device")
                                (check-equal? (s 'set LIGHT 1) #f "method(set); no device")
                                (s 'add-element light-device)
                                (check-equal? (s 'get TEMPERATURE) #f "method(get); wrong device")
                                (check-equal? (s 'set TEMPERATURE 21) #f "method(set); wrong device")
                                (s 'add-element temp-device)
                                (fail (s 'get LIGHT) "method(get); with appropriate device") ;TODO
                                (fail (s 'set LIGHT 1) "method(set); with appropriate device") ;TODO
                                )))
