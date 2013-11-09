#lang r5rs
(#%require rackunit)

(#%require "../internal/instruction-set.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-instruction-set)

(define test-instruction-set (lambda () (test-case
                                         "TEST:instruction-set.rkt"
                                         (check-equal? (instruction-get LIGHT) (list 'GET LIGHT) "instruction(get)")
                                         (check-equal? (instruction-put TEMPERATURE 20) (list 'PUT TEMPERATURE 20) "instruction(put)")
                                         (check-equal? (instruction-set (instruction-get LIGHT) 
                                                                        (instruction-put TEMPERATURE 20)) 
                                                       (list (list 'GET LIGHT)
                                                             (list 'PUT TEMPERATURE 20))
                                                       "instruction(put)"))))