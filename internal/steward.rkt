#lang r5rs
(#%require (only racket/base error)
           (only racket/list empty?)
           (only racket/base current-seconds))

(#%require "device.rkt"
           "sensor.rkt"
           "../communication/action.rkt"
           "../communication/messenger.rkt"
           "element-type.rkt"
           "../rule/rule-manager.rkt"
           "../structure/map.rkt")
(#%provide Steward)
(#%provide new-steward)

;b===c* internal/steward
; NAME
;  steward
; DESCRIPTION
;  Een steward bevindt zich in een kamer en heeft een uniek ip adres.
;  Elke kamer bevat maximum een steward.
;  Een steward beheerst een aantal devices en
;  kan instructions zenden naar de hardware door gebruik te maken van een messenger.
;  Een steward kan bepaalde rules hebben die automatisch instructions zenden wanneer de rule waar is.
;e===

;class name
(define Steward 'steward)

;b===o* steward/new-steward
; NAME
;  new-steward
; DESCRIPTION
;  Maakt een nieuwe steward aan.
; PARAMETERS
;  * room - de naam van de kamer waar de steward zich bevindt. Elke steward is in een verschillende kamer.
;  * ip - het ip adres van deze steward.
; SYNOPSIS
(define (new-steward room ip)
  ;e===
  (define value car)
  (define update-time cdr)
  (define new-udpate-time-value cons)
  (define refresh-time 60)
  (let ((devices '())
        (rule-manager '())
        (element-type-values (new-map)))
    ;b===m* steward/class
    ; NAME
    ;  class
    ; DESCRIPTION
    ;  Geeft de classe terug van dit object.
    ; RETURN VALUE
    ;  symbol - de naam van de classe
    ; SYNOPSIS
    (define (class)
      ; SOURCE
      Steward)
    ;e===
    ;b===m* steward/get-room
    ; NAME
    ;  get-room
    ; DESCRIPTION
    ;  Geeft de naam van de kamer waar de steward zich bevindt terug.
    ; RETURN VALUE
    ;  string - de naam van de kamer.
    ; SYNOPSIS
    (define (get-room)
      ; SOURCE
      room)
    ;e===
    ;b===m* steward/get-ip
    ; NAME
    ;  get-ip
    ; DESCRIPTION
    ;  Geeft het ip adres van deze steward terug.
    ; RETURN VALUE
    ;  string - het ip adres van deze steward.
    ; SYNOPSIS
    (define (get-ip) 
      ; SOURCE
      ip)
    ;e===
    ;b===m* steward/get-devices
    ; NAME
    ;  get-devices
    ; DESCRIPTION
    ;  Een lijst van de devices die deze steward beheert.
    ; RETURN VALUE
    ;  list - de lijst met de beheerste devices.
    ; SYNOPSIS
    (define (get-devices) 
      ; SOURCE
      devices)
    ;e===
    ;b===m* steward/remove-device
    ; NAME
    ;  remove-device
    ; DESCRIPTION
    ;  Verwijdert een bepaalde device van de beheerste devices.
    ; PARAMETERS
    ;  * device - de device die verwijdert moet worden.
    ; RETURN VALUE
    ;  #<void> - als de device verwijderd werd.
    ;  #f - wanneer de device niet door deze steward beheerst is.
    ; SYNOPSIS
    (define (remove-device device)
      ; SOURCE
      (define (loop previous current)
        (cond
          ((eq? '() current) #f) ;not in list
          ((eq? (car current) device)
           (if (eq? '() previous)
               (set! devices (cdr devices))
               (set-cdr! previous (cdr current))))
          (else
           (loop current (cdr current)))))
      (loop '() (get-devices))
      ((new-action (string-append "Removed device \""
                                  (device 'get-name)
                                  "\" from steward \""
                                  (get-room)
                                  "\"")) 'write))
    ;e===
    ;b===m* steward/add-device
    ; NAME
    ;  add-device
    ; DESCRIPTION
    ;  Voeg een device toe aan deze steward.
    ; PARAMETERS
    ;  * device - de device die toegevoegd moet worden.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (add-device device) 
      ; SOURCE
      (set! devices (cons device devices))
      ((new-action (string-append "Added device \""
                                  (device 'get-name)
                                  "\" to steward \""
                                  (get-room)
                                  "\"")) 'write))
    ;e===
    ;b===m* steward/get
    ; NAME
    ;  get
    ; DESCRIPTION
    ;  Zendt een instruction naar de hardware van een beheerste device via een messenger.
    ;  Geeft het antwoord van de harware terug.
    ; PARAMETERS
    ;  * element-type - het element-type die opgevraagd gaat worden.
    ; RETURN VALUE
    ;  integer - de waarde die de hardware gemeten heeft.
    ;  #f - wanneer steward geen device beheerst die het gegeven element-type kan meten.
    ; SYNOPSIS
    (define (get element-type)
      ; SOURCE
      (let loop ((devs (get-devices)))
        (if (empty? devs)
            #f ;no device with elements to match request
            (let ((is ((car devs) 'get element-type)))
              (if is ;result != false means we found a valid result
                  (let ((last-update (update-time (element-type-values 'find element-type)))
                        (last-value (value (element-type-values 'find element-type))))
                    (if (< (- (current-seconds) last-update) refresh-time)
                        last-value
                        (let ((new-value ((send room (car devs) is) 'get-value)))
                          (if new-value
                              (begin
                                (element-type-values 'remove! element-type)
                                (element-type-values 'add! element-type (new-udpate-time-value new-value (current-seconds)))
                                new-value)
                              #f))))
                  (loop (cdr devs)))))))
    ;e===
    ;b===m* steward/set
    ; NAME
    ;  set
    ; DESCRIPTION
    ;  Zendt een instruction naar de hardware van een beheerste device via een messenger.
    ;  Geeft het antwoord van de harware terug.
    ; PARAMETERS
    ;  * element-type - het element-type die aangepast moet worden.
    ;  * value - de waarde waarop element-type aangepast moet worden.
    ; RETURN VALUE
    ;  #t - wanneer de waarde correct werd aangepast.
    ;  #f - wanneer de waarde niet werd aangepast.
    ;  #f - wanneer steward geen device beheerst die het gegeven element-type kan aanpassen.
    ; SYNOPSIS
    (define (set element-type value)
      ; SOURCE
      (let loop ((devs (get-devices)))
        (if (empty? devs)
            #f ;no device with elements to match request
            (let ((is ((car devs) 'set element-type value)))
              (cond
                (is ;result != false means we found a valid result
                 ((new-action (string-append "Set "
                                             (to-string element-type)
                                             " to "
                                             (number->string value)
                                             " in steward \""
                                             (get-room)
                                             "\"")) 'write)
                 (let ((executed? ((send room (car devs) is) 'get-value)))
                   (if executed?
                       (begin
                         (element-type-values 'remove! element-type)
                         (element-type-values 'add! element-type (new-udpate-time-value value (current-seconds)))
                         executed?)
                       #f)))
                (else (loop (cdr devs))))))))
    ;e===
    ;b===m* steward/get-rule-manager
    ; NAME
    ;  get-rule-manager
    ; DESCRIPTION
    ;  Geeft de rule-manager van deze steward terug.
    ; RETURN VALUE
    ;  rule-manager - deze steward's rule manager.
    ; SYNOPSIS
    (define (get-rule-manager)
      ; SOURCE
      rule-manager)
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((get-room) (get-room))
        ((get-ip) (get-ip))
        ((add-device) (apply add-device args))
        ((remove-device) (apply remove-device args))
        ((get) (apply get args))
        ((set) (apply set args))
        ((get-devices) (get-devices)) ;read-only
        ((get-rule-manager) (get-rule-manager))
        (else (error "Error : Steward.class : unknown method : " message))))
    
    (set! rule-manager (new-rule-manager dispatch))
    ;client ports
    (steward-port-map 'add! (get-room) (new-steward-ports dispatch))
    ;init element-type-values
    (for-each-element-type (lambda(et) (element-type-values 'add! et (new-udpate-time-value #f 0))))
    dispatch))