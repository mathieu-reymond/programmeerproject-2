#lang r5rs
(#%require racket/tcp
           (only racket/base let-values)
           (only racket/base date-second)
           (only racket/date current-date))
(#%require "../structure/hash.rkt"
           "../communication/parser.rkt"
           "../internal/instruction.rkt"
           "physical-room.rkt"
           "../communication/xbee-simulation.rkt" ;for simulation
           ;"../communication/xbee.rkt" ;for real hardware
           )

(#%provide new-steward-server)

(define (new-steward-server room)
  (let* ((port (djb2-port room))
         (listen (tcp-listen port))
         (xbee (xbee-initialize "/dev/ttyUSB0" 9600)))
    (let-values (((in out) (tcp-accept listen)))
      (define (process-request)
        (let* ((request (read in))
               (inst (list-to-instruction (cdr request))))
          ;returns answer
          (inst 'execute xbee (car request)) ;(car request) contains device-serial-number
          (if (xbee-ready? xbee) ;an answer came back in the buffer ?
              (begin
                (xbee-tick xbee) ;flush buffer
                (let ((frame (xbee-read-frame xbee))) ;read answer
                  (instruction-to-list (new-instruction-ret frame)) ;temp, should be a bytevector
                  ))
              (instruction-to-list (new-instruction-ret #f))))) ;something went wrong (message lost?), should return NACK
      (define (loop)
        (xbee-discover-nodes xbee) ;TEMP : look for devices
        (let ((ret (process-request)))
          (write ret out)
          (flush-output out))
        (loop))
      (loop)
      )))