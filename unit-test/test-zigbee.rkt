#lang r5rs
(#%require rackunit)

(#%require "../communication/zigbee.rkt"
           "../internal/element-type.rkt"
           "../physical/physical-room.rkt"
           "../physical/hardware-device.rkt")
(#%provide test-zigbee)

(define zigbee-string "GET TEM")
(define zigbee-instruction (vector 71 69 84 32 84 69 77)) ;71=G 69=E 84=T 32=#\space 77=M
(define address64 (vector 1 2 3 4 5 6 7 8))
(define address16 (vector 1 2 3))
(define zigbee-message (vector 144 1 2 3 4 5 6 7 8 1 2 3 71 69 84 32 84 69 77 10 10))

(define room (new-physical-room "room"))
(define hd (new-hardware-device "serial" room))
(define tem (room 'get TEMPERATURE))

(define test-zigbee (lambda() (test-case
                               "TEST:zigbee.rkt"
                               (check-equal? (new-zigbee-instruction zigbee-string) zigbee-instruction "method(new-zigbee-instruction)")
                               (check-equal? (new-zigbee-message zigbee-recieve-paquet address64 address16 zigbee-instruction) 
                                             zigbee-message
                                             "method(new-zigbee-message)")
                               (check-equal? (zigbee-message-to-zigbee-instruction zigbee-message)
                                             zigbee-instruction
                                             "method(zigbee-message-to-zigbee-instruction)")
                               (check-equal? (zigbee-instruction-to-zigbee-string zigbee-instruction)
                                             zigbee-string
                                             "method(zigbee-instruction-to-zigbee-string)")
                               (check-equal? (execute-zigbee-string zigbee-string room) "TEM=20" "method(execute-zigbee-string)")
                               )))