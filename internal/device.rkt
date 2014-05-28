#lang r5rs
(#%require (only racket/base error))
(#%require (only racket/list empty?))

(#%require "actuator.rkt")
(#%require "sensor.rkt")
(#%require "../communication/messenger.rkt")
(#%require "../communication/action.rkt")
(#%provide Device)
(#%provide new-device)
(#%provide device-types)

;b===c* internal/device
; NAME
;  device
; DESCRIPTION
;  Een toestel die een aantal sensoren en actuatoren bevat. 
;  Een device kan de element-types van de kamer waarin hij zich bevindt opvragen en aanpassen,
;  vermits hij bestaat uit de corresponderende sensoren en actuatoren.
;  Elke device heeft een naam en een unieke identifier.
;  Elke device die dezelfde sensoren en actuatoren bevat is beschouwd als hetzelfde type device,
;  en devices die van hetzelfde type zijn hebben ook dezelfde naam.
;  Dus, wanneer een nieuw device aangemaakt wordt en zijn naam hetzelfde is als een
;  van de bestaande devices wordt er automatisch de corresponderende sensoren en actuatoren toegevoegd.
;e===

;Device holds multiple Sensors and Actuators.
;It can get information and modify its environment using those Sensors and Actuators.

(define (new-device-types)
  (let ((devices '()))
    (define (for-each-device-type proc)
      (for-each proc devices))
    (define (contains? device-name)
      (let ((already-exists #f))
        (for-each-device-type (lambda(d) (set! already-exists (or already-exists (equal? (d 'get-name) device-name)))))
        already-exists))
    (define (add-device device)
      (if (not (contains? (device 'get-name)))
          (begin
            (set! devices (cons device devices))
            ((new-action (string-append "Created device \""
                                        (device 'get-name)
                                        "\"")) 'write))
          #f))
    (define (get-device name)
      (let ((device #f))
        (for-each-device-type (lambda(d) (if (equal? name (d 'get-name)) (set! device d) #f)))
        device))
    
    (define (dispatch message . args)
      (case message
        ((for-each-device-type) (apply for-each-device-type args))
        ((contains?) (apply contains? args))
        ((add-device) (apply add-device args))
        ((get-device) (apply get-device args))))
    dispatch))

;b===v* device/device-types
; NAME
;  device-types
; DESCRIPTION
;  Deze variabele houdt verschillende type devices bij.
; SOURCE
(define device-types (new-device-types))
;e===

;class name
(define Device 'device)

;constructor
;@param name : this Device's name
;@param serial-number : this Device's serial-number
;b===o* device/new-device
; NAME
;  new-device
; DESCRIPTION
;  Maakt een nieuw device aan.
; PARAMETERS
;  * name - de naam van het device. Elke device met dezelfde naam is van hetzelfde type.
;  * serial-number - de unieke identifier van deze device.
; SYNOPSIS
(define (new-device name serial-number)
;e===
  (let ((elements '()))
    ;b===m* device/class
    ; NAME
    ;  class
    ; DESCRIPTION
    ;  Geeft de classe terug van dit object.
    ; RETURN VALUE
    ;  symbol - de naam van de classe
    ; SYNOPSIS
    (define (class)
    ; SOURCE
      Device)
    ;e===
    ;b===m* device/get-name
    ; NAME
    ;  get-name
    ; DESCRIPTION
    ;  Geeft de naam van deze device terug.
    ; RETURN VALUE
    ;  string - de naam van deze device.
    ; SYNOPSIS
    (define (get-name)
    ; SOURCE
      name)
    ;e===
    ;b===m* device/get-serial-number
    ; NAME
    ;  get-serial-number
    ; DESCRIPTION
    ;  Geeft de unieke identifier van deze device terug.
    ; RETURN VALUE
    ;  string - de unieke identifier van deze device.
    ; SYNOPSIS
    (define (get-serial-number) 
    ; SOURCE
      serial-number)
    ;e===
    ;b===m* device/get-elements
    ; NAME
    ;  get-elements
    ; DESCRIPTION
    ;  Geeft een lijst terug met de elements van deze device.
    ; RETURN VALUE
    ;  list - Een lijst van de elements die de device bevat.
    ; SYNOPSIS
    (define (get-elements)
    ; SOURCE
      elements)
    ;e===
    ;b===m* device/add-element
    ; NAME
    ;  add-element
    ; DESCRIPTION
    ;  Voeg een nieuw element toe aan deze device.
    ; PARAMETERS
    ;  * element - het element die toegevoegd moet worden.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (add-element element)
    ; SOURCE
      (set! elements (cons element (get-elements))))
    ;e===
    ;b===m* device/get
    ; NAME
    ;  get
    ; DESCRIPTION
    ;  Vraagt informatie over het meegegeven element-type.
    ;  De device zoekt naar een sensor met het corresponderende element-type,
    ;  en geeft dan een instruction-get terug.
    ; PARAMETERS
    ;  * element-type - het element-type waarvan de waarde gekend wilt worden.
    ; RETURN VALUE
    ;  * instruction-get - een instructie als het de gevraagde sensor bevat
    ;  * #f - false als er geen sensor gevonden werd.
    ; SYNOPSIS
    (define (get element-type)
    ; SOURCE
      (let loop ((els (get-elements)))
        (if (empty? els)
            #f ;looped over all elements, no matching for request
            (let ((current (car els)))
              (if (and (equal? (current 'class) Sensor)
                       (equal? ((current 'super) 'get-type) element-type))
                  (let ((is (current 'get-value)))
                    ;send instruction-set to physical device using messenger:send
                    ;((send dispatch is) 'get-value) ;should be a RET-instruction, with response stored in value
                    is
                    )
                  (loop (cdr els)))))))
    ;e===
    ;b===m* device/set
    ; NAME
    ;  set
    ; DESCRIPTION
    ;  Past de waarde van het meegegeven element-type aan.
    ;  De device zoekt naar een actuator met het corresponderende element-type
    ;  en geeft dan een instruction-put terug.
    ; PARAMETERS
    ;  * element-type - het element-type waarvan de waarde aangepast wilt worden.
    ;  * value - de waarde waarop het element-type gezet wilt worden.
    ; RETURN VALUE
    ;  * instruction-put - een instructie als het de gevraagde actuator bevat.
    ;  * #f - false als er geen actuator gevonden werd.
    ; SYNOPSIS
    (define (set element-type value)
    ; SOURCE
      (let loop ((els (get-elements)))
        (if (empty? els)
            #f ;looped over all elements, no matching for request
            (let ((current (car els)))
              (if (and (equal? (current 'class) Actuator)
                       (equal? ((current 'super) 'get-type) element-type))
                  (let ((is (current 'set-value value)))
                    ;send instruction-set to physical device using messenger:send
                    ;((send dispatch is) 'get-value) ;should be a RET-instruction, with response stored in value
                    is
                    )
                  (loop (cdr els)))))))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((get-name) (get-name))
        ((get-serial-number) (get-serial-number))
        ((add-element) (apply add-element args))
        ((get) (apply get args))
        ((set) (apply set args))
        ((get-elements) (get-elements)) ;should be intern method
        (else (error "Error : Device.class : unknown method : " message))))
    
    ;initialize
    (if (device-types 'contains? (get-name))
        (let ((device (device-types 'get-device (get-name))))
          (set! elements (device 'get-elements)))
        (device-types 'add-device dispatch))
    
    dispatch))
