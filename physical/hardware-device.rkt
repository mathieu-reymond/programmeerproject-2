#lang r5rs

(#%require "../structure/map.rkt"
           "../structure/hash.rkt")

(#%provide new-hardware-device
           hardware-device-map)

;b===c* physical/hardware-device
; NAME
;  hardware-device
; DESCRIPTION
;  Deze klasse wordt gebruikt voor het simuleren van
;  het domotica systeem (via xbee-simulation).
;  Het vervangt de echte hardware waarmee de xbee-device
;  van een kamer communiceert.
;e===
;b===v* hardware-device/hardware-device-map
; NAME
;  hardware-device-map
; DESCRIPTION
;  Een map die alle aangemaakte hardware-devices bijhoudt.
;  elke tupel is (serial-number >< hardware-device).
; SOURCE
(define hardware-device-map (new-map))
;e===
(define (new-address64 string)
  (let ((number (djb2 string))
        (vector (make-vector 8)))
    (define (loop current nr nm)
      (cond
        ((eq? current (vector-length vector)) vector)
        (else
         (vector-set! vector current nm)
         (loop (+ current 1) (floor (/ nr 256)) (modulo nr 256)))))
    (loop 0 (floor (/ number 256)) (modulo number 256))))

;b===o* hardware-device/new-hardware-device
; NAME
;  new-hardware-device
; DESCRIPTION
;  Maakt een nieuwe hardware-device object aan.
;  Een hardware device bevindt zich in een bepaalde physical-room
;  en heeft een uniek identifier.
; PARAMETERS
;  * serial-number - het uniek identifier.
;  * room - de physical-room waarin deze physical-device zicht bevindt.
; SYNOPSIS
(define (new-hardware-device serial-number room)
;e===
  (let ((address64 (new-address64 serial-number)))
    ;b===m* hardware-device/get-serial-number
    ; NAME
    ;  get-serial-number
    ; DESCRIPTION
    ;  Geeft de unieke identifier van deze hardware device terug.
    ; RETURN VALUE
    ;  string - de unieke identifier
    ; SYNOPSIS
    (define (get-serial-number)
    ; SOURCE
      serial-number)
    ;e===
    ;b===m* hardware-device/get-address64
    ; NAME
    ;  get-address64
    ; DESCRIPTION
    ;  Geeft het 64bit adres van deze hardware-device terug.
    ; RETURN VALUE
    ;  vector - het 64bit adres.
    ; SYNOPSIS
    (define (get-address64)
    ; SOURCE
      address64)
    ;e===
    ;b===m* hardware-device/get-room
    ; NAME
    ;  get-room
    ; DESCRIPTION
    ;  Geeft de physical-room waarin deze hardware-device zich bevindt terug.
    ; RETURN VALUE
    ;  physical-room - de physical-room van deze hardware-device.
    ; SYNOPSIS
    (define (get-room)
    ; SOURCE
      room)
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((get-serial-number) (get-serial-number))
        ((get-address64) (get-address64))
        ((get-room) (get-room))))
    
    (hardware-device-map 'add! (get-serial-number) dispatch)
    dispatch))
      