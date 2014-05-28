#lang r5rs
(#%require (only racket/base error))
(#%require (only racket/list empty?))

(#%require "../internal/instruction.rkt")
(#%provide (all-defined))

;b===c* communication/parser
; NAME
;  parser
; DESCRIPTION
;  De parser zet instruction objecten om naar een lijst.
;  Deze lijst kan dan verzonden worden via de messenger.
;  Deze klasse kan ook een lijst terug omzetten naar een instruction object.
;e===

;b===m* parser/instruction-to-list
; NAME
;  instruction-to-list
; DESCRIPTION
;  Zet een instruction object om naar een list.
; PARAMETERS
;  instruction - de instruction die omgezet moet worden.
; RETURN VALUE
;  list - een lijst die de instruction voorstelt.
; SYNOPSIS
(define (instruction-to-list instruction)
; SOURCE
  (cond
    ((equal? (instruction 'tag) TAG_GET) (list TAG_GET (instruction 'get-element-type)))
    ((equal? (instruction 'tag) TAG_PUT) (list TAG_PUT (instruction 'get-element-type) (instruction 'get-value)))
    ((equal? (instruction 'tag) TAG_RET) (list TAG_RET (instruction 'get-value)))
    ((equal? (instruction 'tag) TAG_LIST)
     (let loop ((ctr 0)
                (result (list TAG_LIST)))
       (if (equal? ctr (instruction 'nb-of-instructions))
           (reverse result)
           (loop (+ ctr 1) (cons (instruction-to-list (instruction 'get-instruction ctr)) result)))))
    (else (error "Error : Parser.class : unknown tag when trying to convert to list : " (instruction 'tag)))))
;e===

;b===m* parser/list-to-instruction
; NAME
;  list-to-instruction
; DESCRIPTION
;  Zet een lijst die een instructie voorstelt om naar een instruction object.
; PARAMETERS
;  lst - de list die omgezet moet worden.
; RETURN VALUE
;  instruction - het overeenkomstige instruction object.
; SYNOPSIS
(define (list-to-instruction lst)
; SOURCE
  (cond
    ((equal? (car lst) TAG_GET) (new-instruction-get (cadr lst)))
    ((equal? (car lst) TAG_PUT) (new-instruction-put (cadr lst) (caddr lst)))
    ((equal? (car lst) TAG_RET) (new-instruction-ret (cadr lst)))
    ((equal? (car lst) TAG_LIST)
       (let loop ((instr-lst '())
                  (curr (cdr lst)))
         (if (empty? curr)
             (apply new-instruction-list (reverse instr-lst))
             (loop (cons (list-to-instruction (car curr)) instr-lst) (cdr curr)))))
    (else (error "Error : Parser.class : unknown tag when trying to convert to instruction : " (car lst)))))
;e===
