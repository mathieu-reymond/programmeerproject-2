#lang r5rs
(#%require racket/gui/base)
(#%require (only racket/class send))
(#%require (only racket/base thread))
(#%require (only racket/base date current-seconds))
(#%require (only racket/base sleep))

(#%require "internal/element-type.rkt")
(#%require "physical/physical-room.rkt")
(#%require "physical/hardware-device.rkt")
(#%require "gui/gui.rkt")
(#%require "db/manager.rkt")

;database manager
(define manager (new-db-manager "db/domotica.db"))

;central-unit from database
(define central-unit (manager 'restore-state))
;simulate hardware
(central-unit 'for-each-steward (lambda(s)
                                  (let ((room (new-physical-room (s 'get-room))))
                                    (for-each (lambda(d) (new-hardware-device (d 'get-serial-number) room)) (s 'get-devices)))))

;show gui
(send (main-frame central-unit 400 300) show #t)

;;record data
;(define (record-data steward)
;  (let ((time (+ (current-seconds) 3)))
;    (define (loop)
;      (cond
;        ((equal? time (current-seconds))
;         (for-each-element-type (lambda(e)
;                                  (let ((res (steward 'get e)))
;                                    (if res
;                                        (manager 'add-time-value steward e (current-seconds) res)
;                                        'no-sensor-for-element-type))))
;         (set! time (+ (current-seconds) 3))
;         (loop))
;        (else (loop))))
;    (loop)))
;;start a thread for each steward
;(central-unit 'for-each-steward (lambda(s) (thread (lambda() (record-data s)))))