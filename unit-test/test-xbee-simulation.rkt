#lang r5rs
(#%require rackunit)

(#%require "../communication/xbee-simulation.rkt"
           "../communication/zigbee.rkt"
           "../physical/physical-room.rkt"
           "../physical/hardware-device.rkt")
(#%provide test-xbee-simulation)

(define xbee (xbee-initialize "/dev/ttyUSB0" 9600))
(define room (new-physical-room "room"))
(define hd1 (new-hardware-device "first device serial" room))
(define address16 (vector 0 0 0)) ;temp

;test methods
(define ln (map (lambda(key) (list key ((hardware-device-map 'find key) 'get-address64))) (hardware-device-map 'get-keys)))

(xbee-discover-nodes xbee)
(xbee-write xbee (hd1 'get-address64) "GET TEM")
(define true-ready (xbee-ready? xbee))
(xbee-tick xbee)
(define false-ready (xbee-ready? xbee))
(define answer (new-zigbee-message zigbee-recieve-paquet (hd1 'get-address64) address16 (new-zigbee-instruction "TEM=20")))

(define test-xbee-simulation (lambda() (test-case
                                        "TEST:xbee-simulation.rkt"
                                        (check-equal? (xbee-list-nodes) (reverse ln) "method(xbee-list-nodes)")
                                        (check-equal? false-ready #f "method(xbee-tick)")
                                        (check-equal? true-ready #t "method(xbee-tick)")
                                        (check-equal? (xbee-read-frame xbee) answer "method(xbee-read-frame)")
                                        )))