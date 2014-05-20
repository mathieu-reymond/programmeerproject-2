#lang r5rs

(#%require "zigbee.rkt"
           "../physical/hardware-device.rkt")

(#%provide xbee-initialize
           xbee-discover-nodes
           xbee-list-nodes
           xbee-tick
           xbee-ready?
           xbee-read-frame
           xbee-write
           list-node-id-string
           list-node-address64)

;Simulation of Xbee protocol
(define list-nodes '())

(define list-node-id-string car)
(define list-node-address64 cadr)

(define (xbee-initialize port rate)
  (let ((buffer '())
        (frames '()))
    (define (add-to-buffer message) (set! buffer (cons message buffer)))
    (define (tick)
      (for-each (lambda(m) (set! frames (cons m frames))) buffer)
      (set! buffer '()))
    (define (read-frame)
      (if (eq? '() frames)
          #f
          (let ((frame (car frames)))
            (set! frames (cdr frames))
            frame)))
    (define (ready?)
      (not (eq? '() buffer)))
    
    (define (dispatch message . args)
      (case message
        ((add-to-buffer) (apply add-to-buffer args))
        ((tick) (tick))
        ((ready?) (ready?))
        ((read-frame) (read-frame))))
    dispatch))

(define (xbee-discover-nodes xbee)
  (let* ((keys (hardware-device-map 'get-keys))
         (list (map (lambda(key) (list key ((hardware-device-map 'find key) 'get-address64))) keys)))
    (set! list-nodes '())
    (for-each (lambda(n) (set! list-nodes (cons n list-nodes))) list)))
(define (xbee-list-nodes) list-nodes)
(define (xbee-tick xbee)
  (xbee 'tick))
(define (xbee-ready? xbee)
  (xbee 'ready?))
(define (xbee-read-frame xbee)
  (xbee 'read-frame))
(define (xbee-write xbee target message)
  (define (find-hardware-device current)
    (cond
      ((eq? '() current) #f) ;no device with this address
      ((eq? (list-node-address64 (car current)) target) (hardware-device-map 'find (list-node-id-string (car current))))
      (else (find-hardware-device (cdr current)))))
  (let ((hardware-device (find-hardware-device (xbee-list-nodes))))
    ;(display "hardware device : ") (display hardware-device) (newline)
    (if hardware-device
        (xbee 'add-to-buffer
              (new-zigbee-message zigbee-recieve-paquet
                                  (hardware-device 'get-address64)
                                  (vector 0 0 0) ;address16
                                  (new-zigbee-instruction (execute-zigbee-string message (hardware-device 'get-room)))))
        #f)))
