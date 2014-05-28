#lang r5rs
(#%require racket/tcp
           (only racket/base let-values)
           (only racket/base sleep))

(#%require "parser.rkt"
           "../structure/map.rkt"
           "../structure/hash.rkt"
           "../physical/hardware-device.rkt")

(#%provide send
           new-steward-ports
           steward-port-map)

;b===v* communication/steward-port-map
; NAME
;  steward-port-map
; DESCRIPTION
;  Een map die de stewards en hun input en output ports bijhoudt.
;  Elke tupel is (steward-ip >< (cons input-port output-port))
; SOURCE
(define steward-port-map (new-map))
;e===

(define steward-in-port car)
(define steward-out-port cdr)
(define (new-steward-ports steward)
  (let-values (((in out) (tcp-connect (steward 'get-ip) ;test with "localhost", Pi address : 192.9.200.133
                                      (djb2-port (steward 'get-room)))))
    (cons in out)))

;b===c* communication/messenger
; NAME
;  messenger
; DESCRIPTION
;  De messenger handelt de communicatie tussen de central-unit en de steward-server af.
;  Instructies worden via tcp/ip verzonden, de steward-server voert de instruction dan uit
;  en geeft een antwoord terug die door de messenger gelezen wordt.
;e===
;b===m* messenger/send
; NAME
;  send
; DESCRIPTION
;  Zendt een instructie naar een specifieke steward-server via een tcp port.
; PARAMETERS
;  * steward-room - de naam van de kamer van de ontvanger
;  * device - de device waarop de instructie toegepast moet worden.
;  * instruction - de instructie die toegepast moet worden.
; RETURN VALUE
;  intsruction - het antwoord van de steward-server
; SYNOPSIS
(define (send steward-room device instruction)
; SOURCE
  (let ((ports (steward-port-map 'find steward-room))
        (parsed-instruction (instruction-to-list instruction)))
    (write (cons (device 'get-serial-number) parsed-instruction) (steward-out-port ports))
    (flush-output (steward-out-port ports))
    (let ((response (read (steward-in-port ports))))
      (display response) (newline)
      (list-to-instruction response))))
;e===
