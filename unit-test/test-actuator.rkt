#lang r5rs
(#%require rackunit)

(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction-set.rkt")
(#%provide test-actuator)

(define a (new-actuator LIGHT))

(define test-actuator (lambda () (test-case
                                  "TEST:actuator.rkt"
                                  (check-equal? (el 'class) Actuator "method(class)")
                                  (check-equal? ((el 'super) 'get-type) LIGHT "method(super)")
                                  (check-equal? (el 'set-value 20) (instruction-put LIGHT 20) "method(set-value)")
                                  )))
