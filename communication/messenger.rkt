#lang r5rs

(#%require "parser.rkt")
(#%require "../physical/hardware-device.rkt")
(#%provide send)

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
  (let ((ports (hardware-device/port-map 'get-ports-for-hardware-device (device 'get-name))) ;the ports to communicate with the corresponding hardware-device
        (parsed-instruction (instruction-to-list instruction))) ;need to parse to send over port
    (write parsed-instruction (cdr ports)) ;send instruction over port
    (let ((response (read (car ports)))) ;get response from hardware-device
      (list-to-instruction response)))) ;returns response as an instruction
