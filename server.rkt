#lang r5rs

(#%require "structure/hostname.rkt"
           "physical/steward-server.rkt"
           "physical/physical-room.rkt"
           "physical/hardware-device.rkt")

(display "==Setup==")
(newline)
(display "Which room are you in ? ")
(define room-name (symbol->string (read)))
(display "Your IP address is : ")
(display (get-ipv4-addrs))
(newline)
(display "==End Setup==")
(newline)

;simulate devices in room
;(define room (new-physical-room room-name))
;(new-hardware-device "123456789" room)
(new-steward-server room-name)