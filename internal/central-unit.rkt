#lang r5rs
(#%require (only racket/base error))

(#%require "steward.rkt")
(#%require "../structure/map.rkt")
(#%require "../communication/action.rkt")
(#%provide CentralUnit)
(#%provide new-central-unit)

(define CentralUnit 'central-unit)

;b===c* internal/central-unit
; NAME
;  central-unit
; DESCRIPTION
;  De central-unit bevat de verschillende stewards van het domotica systeem.
;  Alle interactie die met de gebruiker via de GUI gebeurt wordt in de
;  achtergrond door de central-unit en zijn stewards afgehandeld.
;e===
;b===o* central-unit/new-central-unit
; NAME
;  new-central-unit
; DESCRIPTION
;  Maakt een nieuwe central-unit aan.
; SYNOPSIS
(define (new-central-unit)
;e===
  (let ((stewards (new-map)))
    ;b===m* central-unit/class
    ; NAME
    ;  class
    ; DESCRIPTION
    ;  Geeft de classe terug van dit object.
    ; RETURN VALUE
    ;  symbol - de naam van de classe
    ; SYNOPSIS
    (define (class)
    ; SOURCE
      CentralUnit)
    ;e===
    ;b===m* central-unit/get-stewards
    ; NAME
    ;  get-stewards
    ; DESCRIPTION
    ;  Geeft de lijst van stewards die deze central-unit beheerst terug.
    ; RETURN VALUE
    ;  list - de stewards van deze central-unit.
    ; SYNOPSIS
    (define (get-stewards)
    ; SOURCE
      (stewards 'get-elements))
    ;e===
    ;b===m* central-unit/for-each-steward
    ; NAME
    ;  for-each-steward
    ; DESCRIPTION
    ;  Past een procedure toe op elke steward van deze central-unit.
    ; PARAMETERS
    ;  proc - de procedure die toegepast gaat worden.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (for-each-steward proc)
    ; SOURCE
      (for-each proc (get-stewards)))
    ;e===
    ;b===m* central-unit/add-steward
    ; NAME
    ;  add-steward
    ; DESCRIPTION
    ;  Voeg een steward toe.
    ; PARAMETERS
    ;  steward - steward die toegevoegd moet worden.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (add-steward steward)
    ; SOURCE
      (stewards 'add! (steward 'get-room) steward)
      ((new-action (string-append "Added new Steward \""
                                 (steward 'get-room)
                                 "\" to Domotica")) 'write))
    ;e===
    ;b===m* central-unit/get-steward
    ; NAME
    ;  get-steward
    ; DESCRIPTION
    ;  Zoek naar de steward van een bepaalde kamer.
    ; PARAMETERS
    ;  room - de kamer van de gezochte steward.
    ; RETURN VALUE
    ;  steward - de gevonden steward.
    ;  #f - false wanneer er geen overeenkomstige steward is.
    ; SYNOPSIS
    (define (get-steward room)
    ; SOURCE
      (stewards 'find room))
    ;e===
    ;b===m* central-unit/remove-steward
    ; NAME
    ;  remove-steward
    ; DESCRIPTION
    ;  Verwijder een specifieke steward.
    ;  Deze wordt niet langer beheerst door de central-unit.
    ; PARAMETERS
    ;  steward - de steward die verwijdert moet worden.
    ; RETURN VALUE
    ;  #<void> - wanneer de steward verwijderd werd.
    ;  #f - false wanneer er geen overeenkomstige steward is.
    ; SYNOPSIS
    (define (remove-steward steward)
    ; SOURCE
      (stewards 'remove! (steward 'get-room))
      ((new-action (string-append "Removed Steward \""
                                 (steward 'get-room)
                                 "\" from Domotica")) 'write))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((class) (class))
        ((for-each-steward) (apply for-each-steward args))
        ((add-steward) (apply add-steward args))
        ((get-steward) (apply get-steward args))
        ((get-stewards) (get-stewards))
        ((remove-steward) (apply remove-steward args))
        (else (error "Error : CentralUnit.class : unknown method : " message))))
    
    dispatch))
