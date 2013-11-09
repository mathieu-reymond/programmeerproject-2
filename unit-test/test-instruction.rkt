#lang r5rs
(#%require rackunit)

(#%require "../internal/instruction.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-instruction)

(define i-get (new-instruction-get LIGHT))
(define i-put (new-instruction-put LIGHT 1))
(define i-list (new-instruction-list i-get i-put))

(define test-instruction (lambda () (test-case
                                         "TEST:instruction.rkt"
                                         (check-equal? (i-get 'tag) TAG_GET "method(tag)")
                                         (check-equal? (i-put 'tag) TAG_PUT "method(tag)")
                                         (check-equal? (i-list 'tag) TAG_LIST "method(tag)")
                                         (check-equal? (i-get 'get-element-type) LIGHT "method(get-element-type)")
                                         (check-equal? (i-put 'get-value) 1 "method(get-value)")
                                         (check-equal? (i-list 'nb-of-instructions) 2 "method(nb-of-instructions)")
                                         (check-equal? (i-list 'get-instruction 1) i-put "method(get-instruction)"))))