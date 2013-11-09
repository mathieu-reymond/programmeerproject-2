#lang r5rs

(#%provide (all-defined))

;A set of instructions
;@param instructions : a sequence of instructions
(define (instruction-set . instructions) instructions)

;the GET instruction
;@param element-type : the type you want to get (temperature, light ...)
(define (instruction-get element-type) (list 'GET element-type))

;the SET instruction
;@param element-type : the type you want to put
;@param value : the value you want it to be put
(define (instruction-put element-type value) (list 'PUT element-type value))
