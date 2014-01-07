#lang r5rs
(#%require rackunit)

(#%require "../physical/hardware-device.rkt")
(#%require "../physical/physical-room.rkt")
(#%require "../communication/parser.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction.rkt")
(#%provide test-hardware-device)

(define room (new-physical-room "room"))
(define hd1 (new-hardware-device "first device serial" room))
(define hd2 (new-hardware-device "second device serial" room))

(define hd1-ports (hardware-device/port-map 'get-ports-for-hardware-device "first device serial"))
(define hd2-ports (hardware-device/port-map 'get-ports-for-hardware-device "second device serial"))

(write (instruction-to-list (new-instruction-get LIGHT)) (cdr hd1-ports))
(write (instruction-to-list (new-instruction-put LIGHT 1)) (cdr hd2-ports))

(define test-hardware-device (lambda () (test-case
                                         "TEST:hardware-device.rkt"
                                         (check-equal? (hd1 'get-serial-number) "first device serial")
                                         (check-equal? (read (car hd1-ports)) (instruction-to-list (new-instruction-ret 1)) "method(process-request)")
                                         (check-equal? (read (car hd2-ports)) (instruction-to-list (new-instruction-ret #t)) "method(process-request)")
                                         )))