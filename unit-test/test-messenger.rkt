#lang r5rs
(#%require rackunit)

(#%require "../communication/messenger.rkt")
(#%require "../communication/parser.rkt")
(#%require "../internal/instruction.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../physical/hardware-device.rkt")
(#%require "../physical/physical-room.rkt")
(#%provide test-messenger)

(define d (new-device "name" "serial-number"))
(define hd (new-hardware-device (d 'get-serial-number) "room"))

(define test-messenger (lambda () (test-case
                                   "TEST:messenger.rkt"
                                   (check-equal? (instruction-to-list (send d (new-instruction-get LIGHT)))
                                                 (instruction-to-list (new-instruction-ret 1))
                                                 "method(send)"))))
