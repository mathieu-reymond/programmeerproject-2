#lang r5rs
(#%require rackunit)

(#%require "../communication/xbee-simulation.rkt"
           "../communication/zigbee.rkt"
           "../physical/physical-room.rkt"
           "../physical/hardware-device.rkt")
(#%provide test-xbee-simulation)

(define xbee (xbee-initialise "/dev/ttyUSB0" 9600))
(define room (new-physical-room "room"))
(define hd1 (new-hardware-device "first device serial" room))
(define address16 (vector 0 0)) ;temp
(define options 0)

;test methods

(xbee-discover-nodes xbee)
(xbee-write xbee (hd1 'get-address64) (vector 71 69 84 32 84 69 77 10)) ;71=G 69=E 84=T 32=#\space 77=M
(define true-ready (xbee-ready? xbee))
(xbee-tick xbee)
(xbee-read-frame xbee) ;skip the delivered status message
(define false-ready (xbee-ready? xbee))
(define answer (new-zigbee-message zigbee-recieve-paquet (hd1 'get-address64) address16 options ((new-zigbee-instruction "POW=ON\nTEM=20\n") 'to-vector)))

;(define ln (map (lambda(key) (list key ((hardware-device-map 'find key) 'get-address64))) (hardware-device-map 'get-keys)))
(define (contains? lst el)
  (let ((contains #f))
    (for-each (lambda(e) (set! contains (or contains (equal? e el)))) lst)
    contains))
(define node (list (hd1 'get-serial-number) (hd1 'get-address64)))

(define test-xbee-simulation (lambda() (test-case
                                        "TEST:xbee-simulation.rkt"
                                        (check-equal? (contains? (xbee-list-nodes) node) #t "method(xbee-list-nodes)")
                                        (check-equal? false-ready #f "method(xbee-tick)")
                                        (check-equal? true-ready #t "method(xbee-tick)")
                                        (check-equal? (xbee-read-frame xbee) (answer 'to-vector) "method(xbee-read-frame)")
                                        )))