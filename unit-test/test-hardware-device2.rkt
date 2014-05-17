#lang r5rs
(#%require rackunit)

(#%require "../physical/hardware-device2.rkt")
(#%require "../physical/physical-room.rkt")
(#%require "../communication/parser.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction.rkt")
(#%provide test-hardware-device)

(define room (new-physical-room "room"))
(define hd1 (new-hardware-device "first device serial" room))
(define hd1-a64 (vector 221 84 48 254 133 190 62 15))
(define hd2 (new-hardware-device "second device serial" room))



(define test-hardware-device (lambda () (test-case
                                         "TEST:hardware-device.rkt"
                                         (check-equal? (hd1 'get-serial-number) "first device serial" "method(get-serial-number)")
                                         (check-equal? hd1-a64 (hd1 'get-address64) "method(get-address64)")
                                         (check-equal? (hd1 'get-room) room "method(get-room)")
                                         (check-equal? (hardware-device-map 'find "first device serial") hd1 "hardware-device-map")
                                         )))