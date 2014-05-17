#lang r5rs
(#%require rackunit)

(#%require "../communication/xbee-simulation.rkt"
           "../communication/zigbee.rkt"
           "../physical/physical-room.rkt"
           "../physical/hardware-device2.rkt")
(#%provide test-xbee-simulation)

(define xbee (xbee-initialize "/dev/ttyUSB0" 9600))
(define room (new-physical-room "room"))
(define hd1 (new-hardware-device "first device serial" room))
(define address16 (vector 1 2 3)) ;temp

;test methods
;(define ln (map (lambda(key) (list key ((hardware-device-map 'find key) 'get-address64))) (hardware-device-map 'get-keys)))
(define (list-nodes-equal? ln)
  (let ((eq #t))
    (for-each (lambda(key) (let ((ieq #f))
                             (for-each (lambda(n) (set! ieq (or ieq
                                                                (equal? n (list key ((hardware-device-map 'find key) 'get-address64))))))
                                       ln)
                             (set! eq (and eq ieq))))
              (hardware-device-map 'get-keys))
    eq))
(xbee-discover-nodes xbee)
(xbee-write xbee (hd1 'get-address64) "GET TEM")
(define true-ready (xbee-ready? xbee))
(xbee-tick xbee)
(define false-ready (xbee-ready? xbee))
(define answer (new-zigbee-message zigbee-recieve-paquet (hd1 'get-address64) address16 (new-zigbee-instruction "TEM=20")))

(define test-xbee-simulation (lambda() (test-case
                                        "TEST:xbee-simulation.rkt"
                                        (check-equal? (list-nodes-equal? (xbee-list-nodes)) #t "method(xbee-list-nodes)")
                                        (check-equal? false-ready #f "method(xbee-tick)")
                                        (check-equal? true-ready #t "method(xbee-tick)")
                                        (check-equal? (xbee-read-frame xbee) answer "method(xbee-read-frame)")
                                        )))