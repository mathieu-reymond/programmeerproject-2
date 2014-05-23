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

(define (new-steward-server room)
  (let* ((port (djb2-port room))
         (listen (tcp-listen port))
         (xbee (xbee-initialise "/dev/ttyUSB0" 9600)))
    (let-values (((in out) (tcp-accept listen)))
      (define (process-request)
        (let ((request (read in)))
          (if (pair? request)
              (let ((inst (list-to-instruction (cdr request))))
                (define (execute-xbee timeout)
                  ;(xbee-tick xbee)
                  ;(display "execute xbee") (newline)
                  (if (not (xbee-ready? xbee))
                      (if (eq? timeout 20)
                          (instruction-to-list (new-instruction-ret #f)) ;TEMP something went wrong
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
                             (instruction-to-list (new-instruction-ret result))) ; finished reading frames
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
