#lang racket

;Send the instruction-set to the device.
;Open a pipe (input/output port) to communicate with the device,
;convert the instruction-set to a string,
;send the string over the port and await for the result
;convert result back in a valid instruction-set
;return result
;@param device : the device which will recieve the message
;@param instruction-set :the message that will be sent
(define (send device instruction-set) )