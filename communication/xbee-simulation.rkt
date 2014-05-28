#lang r5rs

(#%require "zigbee.rkt"
           "../physical/hardware-device.rkt")

(#%provide xbee-initialise
           xbee-discover-nodes
           xbee-list-nodes
           xbee-tick
           xbee-ready?
           xbee-read-frame
           xbee-write
           list-node-id-string
           list-node-address64)

;b===c* communication/xbee-simulation
; NAME
;  xbee-simulation
; DESCRIPTION
;  Dit is een klasse die het gedrag van een xbee-device nadoet.
;  De methodes van deze klasse zijn dezelfde als de methodes van
;  de xbee library. 
;  Zigbee berichten worden op dezelfde manier behandelt als op een
;  echt xbee toestel.
;e===

;Simulation of Xbee protocol
(define list-nodes '())

(define list-node-id-string car)
(define list-node-address64 cadr)

;b===o* xbee-simulation/xbee-initialise
; NAME
;  xbee-initialise
; DESCRIPTION
;  Maakt een nieuw xbee object aan.
;  Een xbee object heeft een buffer waarin de antwoorden 
;  van berichten opgeslagen worden.
;  De berichten worden van de buffer overgeschreven naar een frame list,
;  die dan door de gebruiker gelezen kan worden.
; PARAMETERS
;  port - de port waarop het xbee device zich bevindt.
;  rate - standaart 9600
; SYNOPSIS
(define (xbee-initialise port rate)
;e===
  (let ((buffer '())
        (frames '()))
    (define (add-to-buffer message) (set! buffer (cons message buffer)))
    (define (tick)
      (for-each (lambda(m) (set! frames (cons m frames))) buffer)
      (let ((buffer-length (length buffer)))
        (set! buffer '())
        buffer-length))
    (define (read-frame)
      (if (eq? '() frames)
          (make-vector 0) ;when nothing in frames, give the empty vector
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
;b===m* xbee-simulation/xbee-discover-nodes
; NAME
;  xbee-discover-nodes
; DESCRIPTION
;  Deze methode zoekt naar hardware-devices waarmee hij kan communiceren
;  en houdt ze in een list bij.
;  Elke hardware-device in de list is voorgesteld als :
;  (cons string-id 64bit-address)
;  Om de list op te vragen wordt de methode xbee-list-nodes gebruikt.
; PARAMETERS
;  xbee - het xbee toetsel die naar de hardware gaat zoeken.
; RETURN VALUE
;  #<void>
; SYNOPSIS
(define (xbee-discover-nodes xbee)
; SOURCE
  (let* ((keys (hardware-device-map 'get-keys))
         (list (map (lambda(key) (list key ((hardware-device-map 'find key) 'get-address64))) keys)))
    (set! list-nodes '())
    (for-each (lambda(n) (set! list-nodes (cons n list-nodes))) list)))
;e===
;b===m* xbee-simulation/xbee-list-nodes
; NAME
;  xbee-list-nodes
; DESCRIPTION
;  Geeft de list van gevonden hardware terug
;  die opgezocht werd door xbee-discover-nodes.
; RETURN VALUE
;  list - de list van gevonden nodes
; SYNOPSIS
(define (xbee-list-nodes)
; SOURCE
  list-nodes)
;e===
;b===m* xbee-simulation/xbee-tick
; NAME
;  xbee-tick
; DESCRIPTION
;  De buffer wordt door deze methode leeggemaakt zodat
;  de zigbee berichten gelezen kunnen worden via xbee-read-frame.
; PARAMETERS
;  xbee - de xbee waarvan de buffer leeggemaakt gaat worden.
; RETURN VALUE
;  #<void>
; SYNOPSIS
(define (xbee-tick xbee)
; SOURCE
  (xbee 'tick))
;e===
;b===m* xbee-simulation/xbee-ready?
; NAME
;  xbee-ready?
; DESCRIPTION
;  Bekijkt of de buffer van een bepaalde xbee leeg is.
; PARAMETERS
;  xbee - de xbee waarvan de buffer bekeken wordt.
; RETURN VALUE
;  #t - true wanneer de buffer niet leeg is.
;  #f - false wanneer de buffer leeg is.
; SYNOPSIS
(define (xbee-ready? xbee)
; SOURCE
  (xbee 'ready?))
;e===
;b===m* xbee-simulation/xbee-read-frame
; NAME
;  xbee-read-frame
; DESCRIPTION
;  Leest de eerste frame van een xbee.
;  Elke frame is een zigbee vector die via
;  new-zigbee-message omgezet kan worden naar een
;  zigbee-message object.
; PARAMETERS
;  xbee - de xbee waarvan de frame gelezen gaat worden.
; RETURN VALUE
;  vector - een vector die een zigbee-message voorstelt.
; SYNOPSIS
(define (xbee-read-frame xbee)
; SOURCE
  (xbee 'read-frame))
;e===
;b===m* xbee-simulation/xbee-write
; NAME
;  xbee-write
; DESCRIPTION
;  Schrijft een zigbee-message naar een xbee-device,
;  die het bericht uitvoert.
;  Eerst wordt er een bericht teruggeschreven naar de xbee
;  die het delivery status van de message toont
;  (van type zigbee-transmit-status of x8b).
;  Daarna wordt het antwoord (van type zigbee-recieve-paquet of x90)
;  geschreven in de buffer.
; PARAMETERS
;  xbee - het toestel die de message gaat uitvoeren.
;  target - het 64bit adres van de hardware waarnaar het bericht verzonden moet worden.
;  message - een vector die een zigbee-message voorstelt.
; RETURN VALUE
;  #<void>
;  #f - false als de hardware van het gegeven adres niet gevonden werd.
; SYNOPSIS
(define (xbee-write xbee target message)
; SOURCE
  (define (find-hardware-device current)
    (cond
      ((eq? '() current) #f) ;no device with this address
      ((eq? (list-node-address64 (car current)) target) (hardware-device-map 'find (list-node-id-string (car current))))
      (else (find-hardware-device (cdr current)))))
  (let ((hardware-device (find-hardware-device (xbee-list-nodes))))
    ;(display "hardware device : ") (display hardware-device) (newline)
    (if hardware-device
        (begin
          (xbee 'add-to-buffer
                ((new-zigbee-message zigbee-transmit-status
                                     1 ;frame-id
                                     (vector 0 0) ;address16
                                     0 ;retry count
                                     0 ;delivered
                                     0) ;discovery
                 'to-vector))
          (xbee 'add-to-buffer
                ((new-zigbee-message zigbee-recieve-paquet
                                     (hardware-device 'get-address64)
                                     (vector 0 0) ;address16
                                     0 ;option
                                     (let ((zinst (new-zigbee-instruction (zigbee-vector-to-zigbee-string message))))
                                       ((new-zigbee-instruction (execute-zigbee-instruction zinst (hardware-device 'get-room)))
                                        'to-vector)))
                 'to-vector)))
        #f)))
;e===
