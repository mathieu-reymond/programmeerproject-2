#lang r5rs
(#%require rackunit)

(#%require "../physical/physical-room.rkt")
(#%require "../internal/element-type.rkt")
(#%provide test-physical-room)

(define room (new-physical-room "room"))
(room 'set TEMPERATURE 19)

(define test-physical-room (lambda () (test-case
                                       "TEST:physical-room.rkt"
                                       (check-equal? (room 'get-name) "room" "method(get-name)")
                                       (check-equal? (room 'get LIGHT) 1 "method(get)")
                                       (check-equal? (room 'get TEMPERATURE) 19 "method(set)"))))

