#lang r5rs
(#%require rackunit)
(#%require (only racket/base define-values))
(#%require (only racket/base make-pipe))

(#%require "../physical/hardware-device.rkt")
(#%require "../communication/parser.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/instruction.rkt")
(#%provide test-hardware-device)

(define-values (in-first out-first) (make-pipe 'first 'first))
(define hd1 (new-hardware-device "first device" "room" in-first out-first))

(define-values (in-second out-second) (make-pipe 'second 'second))
(define hd2 (new-hardware-device "second device" "room" in-second out-second))

(write (instruction-to-list (new-instruction-get LIGHT)) out-first)
(write (instruction-to-list (new-instruction-put LIGHT 1)) out-second)

(hd1 'process-request)
(hd2 'process-request)

(define test-hardware-device (lambda () (test-case
                                         "TEST:hardware-device.rkt"
                                         (check-equal? (hardware-device/port-map 'get-ports-for-hardware-device "first device") 
                                                       (cons in-first out-first) 
                                                       "method(get-ports-for-hardware-device)")
                                         (check-equal? (read in-first) "first device" "method(process-request)")
                                         (check-equal? (read in-second) "second device" "method(process-request)"))))