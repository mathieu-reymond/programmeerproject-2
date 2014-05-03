#lang r5rs
(#%require racket/tcp
           (only racket/base let-values))

(#%require "parser.rkt"
           "../structure/map.rkt"
           "../structure/hash.rkt"
           "../physical/hardware-device.rkt")

(#%provide send
           new-steward-ports
           steward-port-map)

(define steward-port-map (new-map))

(define steward-in-port car)
(define steward-out-port cdr)
(define (new-steward-ports steward)
  (let-values (((in out) (tcp-connect "localhost"
                                      (djb2-port (steward 'get-room)))))
    (cons in out)))



;Send the instruction-set to the device.
;Open a pipe (input/output port) to communicate with the device,
;convert the instruction-set to a string,
;send the string over the port and await for the result
;convert result back in a valid instruction
;return result
;@param device : the device which will recieve the message
;@param instruction :the message that will be sent
;@return isntruction : the response
(define (send device instruction)
  (let ((ports (hardware-device/port-map 'get-ports-for-hardware-device (device 'get-serial-number))) ;the ports to communicate with the corresponding hardware-device
        (parsed-instruction (instruction-to-list instruction))) ;need to parse to send over port
    (write parsed-instruction (cdr ports)) ;send instruction over port
    (let ((response (read (car ports)))) ;get response from hardware-device
      (list-to-instruction response)))) ;returns response as an instruction


(define (send2 steward-room device instruction)
  (let ((ports (steward-port-map 'find steward-room))
        (parsed-instruction (instruction-to-list instruction)))
    (write (cons (device 'get-serial-number) parsed-instruction) (steward-out-port ports))
    (let ((response (read (steward-in-port ports))))
      (list-to-instruction response))))
