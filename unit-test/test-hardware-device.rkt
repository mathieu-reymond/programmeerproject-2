#lang r5rs
(#%require rackunit)

(#%require "../physical/hardware-device.rkt")
(#%require "../communication/parser.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction.rkt")
(#%provide test-hardware-device)

(define hd1 (new-hardware-device "first device" "room"))
(define hd2 (new-hardware-device "second device" "room"))

(define hd1-ports (hardware-device/port-map 'get-ports-for-hardware-device "first device"))
(define hd2-ports (hardware-device/port-map 'get-ports-for-hardware-device "second device"))

(write (instruction-to-list (new-instruction-get LIGHT)) (cdr hd1-ports))
(write (instruction-to-list (new-instruction-put LIGHT 1)) (cdr hd2-ports))

(define test-hardware-device (lambda () (test-case
                                         "TEST:hardware-device.rkt"
                                         (check-equal? (read (car hd1-ports)) "first device" "method(process-request)")
                                         (check-equal? (read (car hd2-ports)) "second device" "method(process-request)"))))