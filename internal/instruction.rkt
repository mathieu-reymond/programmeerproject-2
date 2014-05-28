#lang r5rs
(#%require (only racket/base error))
(#%require "../communication/zigbee.rkt"
           "../communication/xbee-simulation.rkt" ;for simulation
           ;"../communication/xbee.rkt" ;for hardware
           "../internal/element-type.rkt")

(#%provide TAG_GET)
(#%provide new-instruction-get)
(#%provide TAG_PUT)
(#%provide new-instruction-put)
(#%provide TAG_LIST)
(#%provide new-instruction-list)
(#%provide TAG_RET)
(#%provide new-instruction-ret)

(define (string-upcase str)
  (define (loop current res)
    (if (eq? current -1) 
        (apply string res)
        (loop (- current 1) (cons (char-upcase (string-ref str current)) res))))
  (loop (- (string-length str) 1) '()))

;b===c* internal/instruction
; NAME
;  instruction
; DESCRIPTION
;  Een abstracte classe die een instructie voor een bepaald element-type voorstelt.
;  Instructions kunnen geparsed worden zodanig dat ze via een messenger verzonden kunnen worden.
; CHILDREN
;  * instruction-get
;  * instruction-put
;  * instruction-ret
;e===

;b===o* instruction/new-instruction
; NAME
;  new-instruction
; DESCRIPTION
;  Maakt een nieuwe instrucion aan.
; PARAMETERS
;  * tag - het type van de instructie
;  * args - een lijst van optionele argumenten
; SYNOPSIS
(define (new-instruction tag . args)
;e===
  ;b===m* instruction/get-tag
  ; NAME
  ;  get-tag
  ; DESCRIPTION
  ;  Geeft de tag van deze instruction terug.
  ; RETURN VALUE
  ;  symbol - de tag van deze instruction.
  ; SYNOPSIS
  (define (get-tag) 
  ; SOURCE
    tag)
  ;e===
  ;b===m* instruction/get
  ; NAME
  ;  get
  ; DESCRIPTION
  ;  Geeft het i-de optionele argument terug.
  ; PARAMETERS
  ;  * i - de positie van het nodige argument
  ; RETURN VALUE
  ;  any - het i-de argument.
  ; SYNOPSIS
  (define (get i) 
  ; SOURCE
    (list-ref args i))
  ;e===
  
  (define (dispatch message . args)
    (case message
      ((tag) (get-tag))
      ((get) (apply get args))
      (else (error "Error : Instruction.class : unknown method : " message))))
  
  dispatch)

(define TAG_GET 'GET)

;b===c* internal/instruction-get
; NAME
;  instruction-get
; DESCRIPTION
;  Instructie om de waarde van een bepaalde element-type te krijgen.
; PARENTS
;  * instruction
;e===

;b===o* instruction-get/new-instruction-get
; NAME
;  new-instruction-get
; DESCRIPTION
;  Maakt een nieuwe instrucion-get aan.
; PARAMETERS
;  * element-type - het element-type van deze instruction.
; SYNOPSIS
(define (new-instruction-get element-type)
;e===
  (let ((instruction (new-instruction TAG_GET element-type)))
    ;b===m* instruction-get/tag
    ; NAME
    ;  tag
    ; DESCRIPTION
    ;  Geeft de tag van deze instruction terug.
    ; RETURN VALUE
    ;  symbol - de tag van deze instruction.
    ; SYNOPSIS
    (define (tag)
    ; SOURCE
      (instruction 'tag))
    ;e===
    ;b===m* instruction-get/get-element-type
    ; NAME
    ;  get-element-type
    ; DESCRIPTION
    ;  Geeft het get-element-type van deze instruction terug.
    ; RETURN VALUE
    ;  element-type - het element-type van deze instruction.
    ; SYNOPSIS
    (define (get-element-type) 
    ; SOURCE
      (instruction 'get 0))
    ;e===
    ;b===m* instruction-get/execute
    ; NAME
    ;  execute
    ; DESCRIPTION
    ;  Past de instruction toe op een device. 
    ;  De instruction schrijft het nodige zigbee bericht op de xbee.
    ; PARAMETERS
    ;  * xbee - de xbee waarop het bericht geschreven gaat worden.
    ;  * device-serial - het unieke identifier van de device.
    ; RETURN VALUE
    ;  #<void> - wanneer het bericht verzonden werd.
    ;  #f - wanneer de xbee de meegegeven device niet vindt.
    ; SYNOPSIS
    (define (execute xbee device-serial)
    ; SOURCE
      ;the message to be send to the hardware
      (define (message)
        (string-append (string-upcase (symbol->string (tag)))
                       ;" "
                       ;(element-type-zigbee-type-map 'find (get-element-type))
                       "\n"))
      ;find the 64bit address of chosen device
      (define (nodes-loop current)
        (cond
          ((eq? '() current) #f)
          ((equal? (list-node-id-string (car current)) device-serial)
           (list-node-address64 (car current)))
          (else (nodes-loop (cdr current)))))
      ;if device is found, write message
      (let ((address64 (nodes-loop (xbee-list-nodes))))
        (display "WRITE TO : ") (display address64) (newline)
        (if address64
            (xbee-write xbee address64 ((new-zigbee-instruction (message)) 'to-vector))
            #f)))
    ;e===
    ;b===m* instruction-get/value-of
    ; NAME
    ;  value-of
    ; DESCRIPTION
    ;  Geeft de waarde van een zigbee-instruction terug die overeenkomt
    ;  met deze instructie (bv. de waarde van POW als het element-type LIGHT was).
    ; PARAMETERS
    ;  * zigbee-instruction - de instructie die opgevraagd gaat worden
    ; RETURN VALUE
    ;  integer - wanneer de instructie het gevraagde element-type bevat.
    ;  #f - wanneer de instructie niet overeenkomt met het element-type.
    ; SYNOPSIS
    (define (value-of zigbee-instruction)
    ; SOURCE
      (let ((values (zigbee-instruction 'values (get-element-type))))
        (if (or (equal? values '())
                (not values))
            #f
            (car values))))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((tag) (tag))
        ((get-element-type) (get-element-type))
        ((execute) (apply execute args))
        ((value-of) (apply value-of args))
        (else (error "Error : InstructionGet.class : unknown method : " message))))
    
    dispatch))

(define TAG_PUT 'PUT)

;b===c* internal/instruction-put
; NAME
;  instruction-put
; DESCRIPTION
;  Instructie om de waarde van een bepaalde element-type aan te passen.
; PARENTS
;  * instruction
;e===

;b===o* instruction-put/new-instruction-put
; NAME
;  new-instruction-put
; DESCRIPTION
;  Maakt een nieuwe instrucion-put aan.
; PARAMETERS
;  * element-type - het element-type van deze instruction.
;  * value - de waarde waarop het element-type gezet moet worden.
; SYNOPSIS
(define (new-instruction-put element-type value)
;e===
  (let ((instruction (new-instruction TAG_PUT element-type value)))
    ;b===m* instruction-put/tag
    ; NAME
    ;  tag
    ; DESCRIPTION
    ;  Geeft de tag van deze instruction terug.
    ; RETURN VALUE
    ;  symbol - de tag van deze instruction.
    ; SYNOPSIS
    (define (tag)
    ; SOURCE
      (instruction 'tag))
    ;e===
    ;b===m* instruction-put/get-element-type
    ; NAME
    ;  get-element-type
    ; DESCRIPTION
    ;  Geeft het get-element-type van deze instruction terug.
    ; RETURN VALUE
    ;  element-type - het element-type van deze instruction.
    ; SYNOPSIS
    (define (get-element-type)
    ; SOURCE
      (instruction 'get 0))
    ;e===
    ;b===m* instruction-put/get-value
    ; NAME
    ;  get-value
    ; DESCRIPTION
    ;  Geeft de waarde van deze instruction terug.
    ; RETURN VALUE
    ;  integer - waarde van deze instruction
    ; SYNOPSIS
    (define (get-value)
    ; SOURCE
      (instruction 'get 1))
    ;e===
    ;b===m* instruction-put/execute
    ; NAME
    ;  execute
    ; DESCRIPTION
    ;  Past de instruction toe op een device. 
    ;  De instruction schrijft het nodige zigbee bericht op de xbee.
    ; PARAMETERS
    ;  * xbee - de xbee waarop het bericht geschreven gaat worden.
    ;  * device-serial - het unieke identifier van de device.
    ; RETURN VALUE
    ;  #<void> - wanneer het bericht verzonden werd.
    ;  #f - wanneer de xbee de meegegeven device niet vindt.
    ; SYNOPSIS
    (define (execute xbee device-serial)
    ; SOURCE
      ;the message to be send to the hardware
      (define (message)
        (string-append ;(string-upcase (symbol->string (tag)))
         "SET"
         " "
         (element-type-zigbee-type-map 'find (get-element-type))
         "="
         (if (equal? LIGHT (get-element-type))
             (if (equal? (get-value) 0)
                 "OFF"
                 "ON")
             (number->string (get-value)))
         "\n"))
      ;find the 64bit address of chosen device
      (define (nodes-loop current)
        (cond
          ((eq? '() current) #f)
          ((equal? (list-node-id-string (car current)) device-serial)
           (list-node-address64 (car current)))
          (else (nodes-loop (cdr current)))))
      ;if device is found, write message
      (let ((address64 (nodes-loop (xbee-list-nodes))))
        (display "WRITE TO : ") (display address64) (newline)
        (if address64
            (xbee-write xbee address64 ((new-zigbee-instruction (message)) 'to-vector))
            #f)))
    ;e===
    ;b===m* instruction-put/value-of
    ; NAME
    ;  value-of
    ; DESCRIPTION
    ;  Bekijkt of een zigbee-instruction correct werd uitgevoerd
    ; PARAMETERS
    ;  * zigbee-instruction - de instructie die opgevraagd gaat worden
    ; RETURN VALUE
    ;  #t - de instructie werd correct uitgevoerd.
    ;  #f - de instructie werd niet correct uitgevoerd.
    ; SYNOPSIS
    (define (value-of zigbee-instruction)
    ; SOURCE
      (zigbee-instruction 'acknowledged?))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((tag) (tag))
        ((get-element-type) (get-element-type))
        ((get-value) (get-value))
        ((execute) (apply execute args))
        ((value-of) (apply value-of args))
        (else (error "Error : InstructionPut.class : unknown method : " message))))
    
    dispatch))

(define TAG_LIST 'LIST)

;A list of instructions
;@param instructions : a list of instructions
(define (new-instruction-list . instructions)
  (let ((tag TAG_LIST)
        (nb-of-instr (length instructions)))
    (define (get-tag) tag)
    (define (get-instruction i) (list-ref instructions i))
    (define (nb-of-instructions) nb-of-instr)
    
    (define (dispatch message . args)
      (case message
        ((tag) (get-tag))
        ((get-instruction) (apply get-instruction args))
        ((nb-of-instructions) (nb-of-instructions))
        (else (error "Error : InstructionList.class : unknown method : " message))))
    
    dispatch))

(define TAG_RET 'RET)

;b===c* internal/instruction-ret
; NAME
;  instruction-ret
; DESCRIPTION
;  Deze instructie wordt van de steward-server zijn overeenkomstige steward
;  (in de central-unit) verzonden als antwoord op de berichten van de central-unit.
; PARENTS
;  * instruction
;e===

;b===o* instruction-ret/new-instruction-ret
; NAME
;  new-instruction-ret
; DESCRIPTION
;  Maakt een nieuwe instrucion-ret aan.
; PARAMETERS
;  * value - de waarde die naar de central-unit verzonden moet worden.
; SYNOPSIS
(define (new-instruction-ret value)
;e===
  (let ((instruction (new-instruction TAG_RET value)))
    ;b===m* instruction-ret/tag
    ; NAME
    ;  tag
    ; DESCRIPTION
    ;  Geeft de tag van deze instruction terug.
    ; RETURN VALUE
    ;  symbol - de tag van deze instruction.
    ; SYNOPSIS
    (define (tag)
    ; SOURCE
      (instruction 'tag))
    ;e===
    ;b===m* instruction-ret/get-value
    ; NAME
    ;  get-value
    ; DESCRIPTION
    ;  Geeft de waarde van deze instruction terug.
    ; RETURN VALUE
    ;  (or integer #t #f) - waarde van deze instruction
    ; SYNOPSIS
    (define (get-value)
     ; SOURCE
      (instruction 'get 0))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((tag) (tag))
        ((get-value) (get-value))
        (else (error "Error : InstructionRet.class : unknown method : " message))))
    dispatch))
