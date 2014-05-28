#lang r5rs
(#%require racket/tcp
           (only racket/base let-values)
           (only racket/base sleep)
           (only racket/base date-second)
           (only racket/date current-date))
(#%require "../structure/hash.rkt"
           "../communication/parser.rkt"
           "../internal/instruction.rkt"
           "physical-room.rkt"
           "../communication/zigbee.rkt"
           "../communication/xbee-simulation.rkt" ;for simulation
           ;"../communication/xbee.rkt" ;for real hardware
           )

(#%provide new-steward-server)

;b===c* physical/steward-server
; NAME
;  steward-server
; DESCRIPTION
;  De server die op het steward toestel runt.
;  Deze server leest instructions van de messenger,
;  voert ze uit via zijn xbee-device en sendt het
;  antwoord terug als een instruction-ret.
;e===
;b===o* steward-server/new-steward-server
; NAME
;  new-steward-server
; DESCRIPTION
;  Maakt een nieuwe steward-server aan.
;  Een steward-server bevindt zich in een bepaalde kamer,
;  de naam van de kamer moet dezelfde zijn als het overeenkomstige
;  steward object beheerst door de central-unit
;  omdat de naam van de kamer gebruikt wordt om de communicatie port te bepalen.
; PARAMETERS
;  * room - de naam van de kamer waarin de steward-server zich bevindt
; SYNOPSIS
(define (new-steward-server room)
;e===
  (let* ((port (djb2-port room))
         (listen (tcp-listen port))
         (xbee (xbee-initialise "/dev/ttyUSB0" 9600)))
    (let-values (((in out) (tcp-accept listen)))
      (define (process-request)
        (let ((request (read in)))
          (display "REQUEST : ") (display request) (newline)
          (if (pair? request)
              (let ((inst (list-to-instruction (cdr request))))
                (define (execute-xbee timeout)
                  ;(xbee-tick xbee)
                  ;(display "execute xbee") (newline)
                  (if (not (xbee-ready? xbee))
                      (if (eq? timeout 20)
                          (instruction-to-list (new-instruction-ret #f)) ;TEMP something went wrong
                          ;(begin (xbee-tick xbee) (execute-xbee 0))
                          (execute-xbee (+ timeout 1)))
                      (let ((again? #f)
                            (result #f))
                        (define (execute-frame frame)
                          (let ((zigbee-message (new-zigbee-message frame)))
                            (display "FRAME : ")
                            (display frame)
                            (newline)
                            (display "ZIGBEE : ")
                            (cond
                              ((equal? zigbee-message #f) (display "not supported"))
                              ((equal? (zigbee-message 'type) zigbee-transmit-status)
                               (display (zigbee-message 'to-vector))
                               (if (zigbee-message 'delivered?)
                                   (display " / Delivered")
                                   (begin (inst 'execute xbee (car request))
                                          (set! again? #t))))
                              ((equal? (zigbee-message 'type) zigbee-recieve-paquet)
                               (display (zigbee-message 'to-vector))
                               (newline)
                               (display ((zigbee-message 'get-zigbee-instruction) 'to-string))
                               (display (inst 'value-of (zigbee-message 'get-zigbee-instruction)))
                               (set! result (inst 'value-of (zigbee-message 'get-zigbee-instruction)))
                               (display "/ result : ") (display result) (newline)
                               )
                              (else
                               (display (zigbee-message 'to-vector))
                               ))
                            (newline)))
                        (define (frame-loop frame)
                          (cond
                            ((equal? frame (make-vector 0))
                             (if (equal? (xbee-tick xbee) 0)
                                 (instruction-to-list (new-instruction-ret result)) ; finished reading frames
                                 (frame-loop (xbee-read-frame xbee))))
                            (else
                             (execute-frame frame)
                             (frame-loop (xbee-read-frame xbee)))))
                        (xbee-tick xbee)
                        (let ((xbee-result (frame-loop (xbee-read-frame xbee))))
                          (if again?
                              (execute-xbee 0)
                              xbee-result)))))
                ;returns answer
                (inst 'execute xbee (car request)) ;(car request) contains device-serial-number
                (execute-xbee 0))
              (instruction-to-list (new-instruction-ret -1))))) ;TEMP <eof>
      (define (loop)
        ;(display (xbee-list-nodes)) (newline)
        (if (eq? '() (xbee-list-nodes))
            (begin (xbee-discover-nodes xbee) (xbee-tick xbee))
            #f)
        (let ((ret (process-request)))
          (write ret out)
          (flush-output out))
        (loop))
      (xbee-discover-nodes xbee) ;look for devices (only on startup enough?)
      (xbee-tick xbee)
      (loop)
      )))
