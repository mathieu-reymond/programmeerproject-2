#lang r5rs
(#%require rackunit)
(#%require (only racket/base seconds->date))
(#%require (only racket/base current-seconds))
(#%require racket/date)

(#%require "../rule/time-interval.rkt")
(#%require "../rule/recurrence.rkt")
(#%require "../rule/rule.rkt")
(#%require "../internal/steward.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../physical/physical-room.rkt")
(#%require "../physical/hardware-device.rkt")
(#%provide test-rule)

(define room (new-physical-room "room"))

(define light-device (new-device "light-device" "light-serial-number"))
(define light-sensor (new-sensor LIGHT))
(define light-actuator (new-actuator LIGHT))
(light-device 'add-element light-sensor)
(light-device 'add-element light-actuator)
(new-hardware-device (light-device 'get-serial-number) room)

(define s (new-steward "bedroom"))
(s 'add-device light-device)

(define time-interval-true (new-time-interval (seconds->date (current-seconds)) 
                                              (seconds->date (+ (current-seconds) 5)) 
                                              (new-weekly (seconds->date (current-seconds)))))
(define time-interval-false (new-time-interval (seconds->date (+ (current-seconds) 5)) 
                                              (seconds->date (+ (current-seconds) 10)) 
                                              (new-weekly (seconds->date (current-seconds)))))

(define rule-already (new-rule LIGHT 1 time-interval-true))
(define rule-not-interval (new-rule LIGHT 0 time-interval-false))
(define rule-change (new-rule LIGHT 0 time-interval-true))

(define test-rule (lambda () (test-case
                                       "TEST:rule.rkt"
                                       (check-equal? (rule-already 'execute s)
                                                     #f
                                                     "method(execute) already right value")
                                       (check-equal? (rule-not-interval 'execute s)
                                                     #f 
                                                     "method(execute) not in interval")
                                       (check-equal? (rule-change 'execute s)
                                                     #t
                                                     "method(execute) changed vaule")
                                       )))