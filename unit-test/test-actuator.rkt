#lang r5rs
(#%require rackunit)

(#%require "../internal/actuator.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction.rkt")
(#%provide test-actuator)

(define a (new-actuator LIGHT))

(define test-actuator (lambda () (test-case
                                  "TEST:actuator.rkt"
                                  (check-equal? (a 'class) Actuator "method(class)")
                                  (check-equal? ((a 'super) 'get-type) LIGHT "method(super)")
                                  (check-equal? ((a 'set-value 20) 'tag) TAG_PUT "method(set-value)")
                                  )))
