#lang r5rs
(#%require rackunit)

(#%require "../communication/zigbee.rkt"
           "../internal/element-type.rkt"
           "../physical/physical-room.rkt"
           "../physical/hardware-device.rkt")
(#%provide test-zigbee)

(define zigbee-string "GET TEM\n")
(define zigbee-vector (vector 71 69 84 32 84 69 77 10)) ;71=G 69=E 84=T 32=#\space 77=M
(define zigbee-instruction (new-zigbee-instruction zigbee-string))
(define address64 (vector 1 2 3 4 5 6 7 8))
(define address16 (vector 1 2))
(define options 3)
(define zigbee-message-vector (vector 144 1 2 3 4 5 6 7 8 1 2 3 71 69 84 32 84 69 77 10 10))
(define zigbee-message (new-zigbee-message zigbee-recieve-paquet address64 address16 options (zigbee-instruction 'to-vector)))

(define room (new-physical-room "room"))
(define hd (new-hardware-device "serial" room))
(define tem (room 'get TEMPERATURE))

(define test-zigbee (lambda() (test-case
                               "TEST:zigbee.rkt"
                               (check-equal? (zigbee-instruction 'to-vector) zigbee-vector "method(new-zigbee-instruction)")
                               (check-equal? (zigbee-message 'to-vector) 
                                             zigbee-message-vector
                                             "method(new-zigbee-message)")
                               (check-equal? ((zigbee-message 'get-zigbee-instruction) 'to-vector)
                                             (zigbee-instruction 'to-vector)
                                             "method(get-zigbee-instruction)")
                               (check-equal? (zigbee-instruction 'to-string)
                                             zigbee-string
                                             "method(zigbee-instruction-to-zigbee-string)")
                               (check-equal? (execute-zigbee-instruction zigbee-instruction room) "POW=ON\nTEM=20\n" "method(execute-zigbee-string)")
                               )))