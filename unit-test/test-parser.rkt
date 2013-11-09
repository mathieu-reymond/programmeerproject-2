#lang r5rs
(#%require rackunit)

(#%require "../communication/parser.rkt")
(#%require "../internal/instruction.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-parser)

(define i-get (new-instruction-get LIGHT))
(define i-put (new-instruction-put LIGHT 1))
(define i-lst1 (new-instruction-list i-get i-put))
(define i-lst2 (new-instruction-list i-get i-lst1 i-put))

(define test-parser (lambda () (test-case
                                "TEST:parser.rkt"
                                (check-equal? (instruction-to-list i-lst2) 
                                              (list TAG_LIST 
                                                    (list TAG_GET LIGHT)
                                                    (list TAG_LIST
                                                          (list TAG_GET LIGHT)
                                                          (list TAG_PUT LIGHT 1))
                                                    (list TAG_PUT LIGHT 1))
                                              "method(instruction-to-list)")
                                (check-equal? ((((list-to-instruction (list TAG_LIST 
                                                                            (list TAG_GET LIGHT)
                                                                            (list TAG_LIST
                                                                                  (list TAG_GET LIGHT)
                                                                                  (list TAG_PUT LIGHT 1))
                                                                            (list TAG_PUT LIGHT 1))) 'get-instruction 1) 'get-instruction 0) 'tag) 
                                              TAG_GET 
                                              "method(list-to-instruction)")
                                )))