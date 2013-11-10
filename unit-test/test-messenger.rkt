#lang r5rs
(#%require rackunit)
(#%require (only racket/base define-values))
(#%require (only racket/base make-pipe))

(#%require "../communication/messenger.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../physical/hardware-device.rkt")
(#%require "../physical/physical-room.rkt")
(#%provide test-messenger)

(define-values (in out) (make-pipe))
(define d (new-device "name" "serial-number"))
(define hd (new-hardware-device (d 'get-name) in out))

(define test-messenger (lambda () (test-case
                                   "TEST:messenger.rkt"
                                   (fail "(send d (new-instruction-get LIGHT)) method(send)"))));TODO
