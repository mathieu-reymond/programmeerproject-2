#lang r5rs
(#%require racket/gui/base)
(#%require (only racket/class send))
(#%require (only racket/base thread))
(#%require (only racket/base date current-seconds))
(#%require (only racket/base sleep))

(#%require "internal/element-type.rkt"
           "physical/physical-room.rkt"
           "physical/hardware-device.rkt"
           "physical/steward-server.rkt"
           "gui/gui.rkt" 
           "db/manager.rkt")

;database manager
(define manager (new-db-manager "db/domotica.db"))

;Raspberry Pi servers
(thread (lambda() (new-steward-server "bedroom")))
(thread (lambda() (new-steward-server "kitchen")))
(thread (lambda() (new-steward-server "living-room")))

;central-unit from database
(define central-unit (manager 'restore-state))
;simulate hardware
(central-unit 'for-each-steward (lambda(s)
                                  (let ((room (new-physical-room (s 'get-room))))
                                    (for-each (lambda(d) (new-hardware-device (d 'get-serial-number) room)) (s 'get-devices)))))

;show gui
(send (main-frame central-unit 400 300) show #t)

;;record data
(define (record-data)
  (define (record-steward steward)
    (for-each-element-type (lambda(e)
                             (let ((res (steward 'get e)))
                               (if res
                                   (manager 'add-time-value steward e (current-seconds) res)
                                   'no-sensor-for-element-type)))))
  (let ((record? (equal? (modulo (current-seconds) 10) 0))) ;every 10 seconds
    (if record?
        (central-unit 'for-each-steward (lambda(s) (record-steward s)))
        'wait))
  (record-data))
(thread record-data)