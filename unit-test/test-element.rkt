#lang r5rs
(#%require rackunit)

(#%require "../internal/element.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-element)

(define el (new-element TEMPERATURE))

(define test-element (lambda () (test-case
                                 "TEST:element.rkt"
                                 (check-equal? (el 'class) Element "method(class)")
                                 (check-equal? (el 'get-type) TEMPERATURE "method(get-type)")
                                 )))
