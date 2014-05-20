#lang r5rs
(#%require (only racket/base error))
(#%require "../communication/xbee-simulation.rkt" ;for simulation
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

;an abstract Instruction class
;Implementations : InstructionGet, InstructionPut, InstructionList
(define (new-instruction tag . args)
  (define (get-tag) tag)
  (define (get i) (list-ref args i))
  
  (define (dispatch message . args)
    (case message
      ((tag) (get-tag))
      ((get) (apply get args))
      (else (error "Error : Instruction.class : unknown method : " message))))
  
  dispatch)

(define TAG_GET 'GET)

;the GET instruction
;@param element-type : the type you want to get (temperature, light ...)
(define (new-instruction-get element-type)
  (let ((instruction (new-instruction TAG_GET element-type)))
    (define (tag) (instruction 'tag))
    (define (get-element-type) (instruction 'get 0))
    (define (execute xbee device-serial)
      ;the message to be send to the hardware
      (define (message)
        (string-append (string-upcase (symbol->string (tag)))
                       " "
                       (element-type-zigbee-type-map 'find (get-element-type))))
      ;find the 64bit address of chosen device
      (define (nodes-loop current)
        (cond
          ((eq? '() current) #f)
          ((equal? (list-node-id-string (car current)) device-serial)
           (list-node-address64 (car current)))
          (else (nodes-loop (cdr current)))))
      ;if device is found, write message
      (let ((address64 (nodes-loop (xbee-list-nodes))))
        (if address64
            (xbee-write xbee address64 (message))
            #f)))
    
    (define (dispatch message . args)
      (case message
        ((tag) (tag))
        ((get-element-type) (get-element-type))
        ((execute) (apply execute args))
        (else (error "Error : InstructionGet.class : unknown method : " message))))
    
    dispatch))

(define TAG_PUT 'PUT)

;the PUT instruction
;@param element-type : the type you want to put
;@param value : the value you want it to be put
(define (new-instruction-put element-type value)
  (let ((instruction (new-instruction TAG_PUT element-type value)))
    (define (tag) (instruction 'tag))
    (define (get-element-type) (instruction 'get 0))
    (define (get-value) (instruction 'get 1))
    (define (execute xbee device-serial)
      ;the message to be send to the hardware
      (define (message)
        (string-append ;(string-upcase (symbol->string (tag)))
                       "SET"
                       " "
                       (element-type-zigbee-type-map 'find (get-element-type))
                       "="
                       (number->string (get-value))))
      ;find the 64bit address of chosen device
      (define (nodes-loop current)
        (cond
          ((eq? '() current) #f)
          ((equal? (list-node-id-string (car current)) device-serial)
           (list-node-address64 (car current)))
          (else (nodes-loop (cdr current)))))
      ;if device is found, write message
      (let ((address64 (nodes-loop (xbee-list-nodes))))
        (if address64
            (xbee-write xbee address64 (message))
            #f)))
    
    (define (dispatch message . args)
      (case message
        ((tag) (tag))
        ((get-element-type) (get-element-type))
        ((get-value) (get-value))
        ((execute) (apply execute args))
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

;the RET instruction
;@param value the value to store
(define (new-instruction-ret value)
  (let ((instruction (new-instruction TAG_RET value)))
    (define (tag) (instruction 'tag))
    (define (get-value) (instruction 'get 0))
    
    (define (dispatch message . args)
      (case message
        ((tag) (tag))
        ((get-value) (get-value))
        (else (error "Error : InstructionRet.class : unknown method : " message))))
    dispatch))
