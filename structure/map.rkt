#lang r5rs
;(require rnrs/mutable-pairs-6)
(#%require (only racket/base error))

(#%provide new-map)

;b===c* structure/map
; NAME
;  map
; DESCRIPTION
;  De map klasse is een klasse die verschillende tupels bevat.
;  Elke tupel heeft een key (elke key in de map moet uniek zijn) en
;  een element. Een element kan gevonden worden via zijn unieke key.
;  Element is hier gezien als een algemene variabele en heeft geen verband met
;  de element klasse van het internal module.
;e===
;b===o* map/new-map
; NAME
;  new-map
; DESCRIPTION
;  Maakt een nieuwe lege map aan.
; SYNOPSIS
(define (new-map)
;e===
  (let ((structure '()))
    ;b===m* map/add!
    ; NAME
    ;  add!
    ; DESCRIPTION
    ;  Voeg een nieuwe tupel (key, element) toe aan de map.
    ;  De key moet uniek zijn (er bestaat nog geen tupel met deze key).
    ; PARAMETERS
    ;  * key - de key van de tupel.
    ;  * element - het element van de tupel.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (add! key element)
    ; SOURCE
      (set! structure (cons (cons key element) structure)))
    ;e===
    ;b===m* map/remove!
    ; NAME
    ;  remove!
    ; DESCRIPTION
    ;  Verwijder een tupel van de map.
    ; PARAMETERS
    ;  * key - de key van de tupel die verwijdert moet worden.
    ; RETURN VALUE
    ;  #<void> - als de tupel verwijderd werd.
    ;  #f - false als er geen tupel met deze key was.
    ; SYNOPSIS
    (define (remove! key)
    ; SOURCE
      (let loop ((previous '())
                 (current structure))
        (cond
          ((eq? '() current) #f)
          ((equal? key (car (car current)))
           (if (eq? '() previous)
               (set! structure (cdr current))
               (set-cdr! previous (cdr current))))
          (else (loop current (cdr current))))))
    ;e===
    ;b===* map/find
    ; NAME
    ;  find
    ; DESCRIPTION
    ;  Vindt een bepaalde tupel een geeft het element ervan terug.
    ; PARAMETERS
    ;  * key - de key van de gezochte tupel.
    ; RETURN
    ;  any - het element van de tupel.
    ; SYNOPSIS
    (define (find key)
    ; SOURCE
      (let loop ((current structure))
        (cond
          ((eq? '() current) #f)
          ((equal? key (car (car current)))
           (cdr (car current)))
          (else (loop (cdr current))))))
    ;e===
    ;b===m* map/get-elements
    ; NAME
    ;  get-elements
    ; DESCRIPTION
    ;  geeft een list terug met alle elements die deze map bevat.
    ; RETURN VALUE
    ;  list - een lijst met alle elements van deze map.
    ; SYNOPSIS
    (define (get-elements)
    ; SOURCE
      (map (lambda (el) (cdr el)) structure))
    ;e===
    ;b===m* map/get-keys
    ; NAME
    ;  get-keys
    ; DESCRIPTION
    ;  geeft een list terug met alle keys die deze map bevat.
    ; RETURN VALUE
    ;  list - een lijst met alle keys van deze map.
    ; SYNOPSIS
    (define (get-keys)
    ; SOURCE
      (map (lambda (el) (car el)) structure))
    ;e===
    ;b===m* map/key
    ; NAME
    ;  key
    ; DESCRIPTION
    ;  Zoek naar een van de tupels die element bevat en geeft daarvan de key terug.
    ;  Als er verschillende tupels zijn met hetzelfde element wordt maar een van de keys teruggegeven.
    ; PARAMETERS
    ;  * element - het element van de gezochte tupel.
    ; RETURN VALUE
    ;  any - de key van de gevonden tupel.
    ;  #f - false als geen tupel gevonden werd.
    ; SYNOPSIS
    (define (key element)
    ; SOURCE
      (define (loop keys)
        (cond
          ((eq? keys '()) #f) ;no such element in map
          ((equal? (find (car keys)) element)
           (car keys))
          (else (loop (cdr keys)))))
      (loop (get-keys)))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((add!) (apply add! args))
        ((remove!) (apply remove! args))
        ((find) (apply find args))
        ((key) (apply key args))
        ((get-elements) (get-elements))
        ((get-keys) (get-keys))
        (else (error "Error : Map.class : unknown method : " message))))
    
    dispatch))