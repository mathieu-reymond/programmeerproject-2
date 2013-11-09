#lang racket

;Send the instruction-set to the device.
;Open a pipe (input/output port) to communicate with the device,
;convert the instruction-set to a string,
;send the string over the port and await for the result
;convert result back in a valid instruction-set
;return result
;@param device : the device which will recieve the message
;@param instruction-set :the message that will be sent
(define (send device instruction-set) #f)



;EX unsing pipes
(define (p-dev-test name in out)
  (let ((res (read in)))
    (write (string-append "ack:" res) out)))
(define-values (in out) (make-pipe))
(write "hello" out)
(p-dev-test "bla" in out)
(read in)