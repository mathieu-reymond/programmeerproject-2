#lang r5rs
(#%require rackunit)
(#%require (only racket/base seconds->date)
           (only racket/base current-seconds)
           (only racket/base thread)
           racket/date)

(#%require "../rule/time-interval.rkt"
           "../rule/recurrence.rkt"
           "../rule/rule.rkt"
           "../internal/steward.rkt"
           "../internal/device.rkt"
           "../internal/sensor.rkt"
           "../internal/actuator.rkt"
           "../internal/element-type.rkt"
           "../physical/physical-room.rkt"
           "../physical/hardware-device.rkt"
           "../physical/steward-server.rkt")
(#%provide test-rule)

(define rule-room (new-physical-room "rule"))
(thread (lambda() (new-steward-server "ruleroom")))

(define light-device (new-device "rule-light-device" "rule-light-serial-number"))
(define light-sensor (new-sensor LIGHT))
(define light-actuator (new-actuator LIGHT))
(light-device 'add-element light-sensor)
(light-device 'add-element light-actuator)
(new-hardware-device (light-device 'get-serial-number) rule-room)

(define s (new-steward "ruleroom" "localhost"))
(s 'add-device light-device)

(define time-interval-true (new-time-interval (seconds->date (current-seconds)) 
                                              ;(seconds->date (+ (current-seconds) 5)) 
                                              (new-recurrence "weekly" (seconds->date (current-seconds)))))
(define time-interval-false (new-time-interval (seconds->date (+ (current-seconds) 5)) 
                                              ;(seconds->date (+ (current-seconds) 10)) 
                                              (new-recurrence "weekly" (seconds->date (current-seconds)))))

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