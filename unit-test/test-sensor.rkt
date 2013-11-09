#lang r5rs
(#%require rackunit)

(#%require "../internal/sensor.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction-set.rkt")
(#%provide test-sensor)

(define s (new-sensor LIGHT))

(define test-sensor (lambda () (test-case
                                "TEST:sensor.rkt"
                                (check-equal? (s 'class) Sensor "method(class)")
                                (check-equal? ((s 'super) 'get-type) LIGHT "method(super)")
                                (check-equal? (s 'get-value) (instruction-get LIGHT) "method(get-value)")
                                )))
