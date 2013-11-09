#lang r5rs
(#%require (only racket/base error))
(#%require (only racket/list empty?))

(#%require "../internal/instruction.rkt")
(#%provide (all-defined))

;convert an instruction to a list (which is sendable over ports)
(define (instruction-to-list instruction)
  (cond
    ((equal? (instruction 'tag) TAG_GET) (list TAG_GET (instruction 'get-element-type)))
    ((equal? (instruction 'tag) TAG_PUT) (list TAG_PUT (instruction 'get-element-type) (instruction 'get-value)))
    ((equal? (instruction 'tag) TAG_LIST)
     (let loop ((ctr 0)
                (result (list TAG_LIST)))
       (if (equal? ctr (instruction 'nb-of-instructions))
           (reverse result)
           (loop (+ ctr 1) (cons (instruction-to-list (instruction 'get-instruction ctr)) result)))))
    (else (error "Error : Parser.class : unknown tag when trying to convert to list : " (instruction 'tag)))))

;convert a valid list to an instruction object
(define (list-to-instruction lst)
  (cond
    ((equal? (car lst) TAG_GET) (new-instruction-get (cadr lst)))
    ((equal? (car lst) TAG_PUT) (new-instruction-put (cadr lst) (caddr lst)))
    ((equal? (car lst) TAG_LIST)
       (let loop ((instr-lst '())
                  (curr (cdr lst)))
         (if (empty? curr)
             (apply new-instruction-list (reverse instr-lst))
             (loop (cons (list-to-instruction (car curr)) instr-lst) (cdr curr)))))
    (else (error "Error : Parser.class : unknown tag when trying to convert to instruction : " (car lst)))))
