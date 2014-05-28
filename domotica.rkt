#lang r5rs
(#%require racket/gui/base
           racket/date
           (only racket/class send)
           (only racket/base thread)
           (only racket/base date current-seconds seconds->date)
           (only racket/base sleep))

(#%require "internal/element-type.rkt"
           "physical/physical-room.rkt"
           "physical/hardware-device.rkt"
           "physical/steward-server.rkt"
           "gui/gui.rkt" 
           "db/manager.rkt")

;database manager
(define manager (new-db-manager "db/domotica.db"))

;====Raspberry Pi servers====
(thread (lambda() (new-steward-server "bedroom")))
(thread (lambda() (new-steward-server "kitchen")))
(thread (lambda() (new-steward-server "living-room")))
;============================

;central-unit from database
(define central-unit (manager 'restore-state))

;====simulate hardware====
(central-unit 'for-each-steward (lambda(s)
                                  (let ((room (new-physical-room (s 'get-room))))
                                    (for-each (lambda(d) (new-hardware-device (d 'get-serial-number) room)) (s 'get-devices)))))
;=========================

;show gui
(send (main-frame central-unit 400 300) show #t)

(define wait-time 10) ;10 seconds
;record data
;(define (record-data)
;  (define (record-steward steward)
;    (for-each-element-type (lambda(e)
;                             (let ((res (steward 'get e)))
;                               (if res
;                                   (manager 'add-time-value steward e (current-seconds) res)
;                                   'no-sensor-for-element-type)))))
;  (let ((record? (equal? (modulo (current-seconds) wait-time) 0))) ;every "wait-time" seconds
;    (if record?
;        (central-unit 'for-each-steward (lambda(s) (record-steward s)))
;        'wait))
;  (record-data))
;(thread record-data)
;apply rules
(define (apply-rules)
  (define (loop applied)
    (let ((applied? applied)
          (record? (equal? (modulo (current-seconds) wait-time) 0))) ;every "wait-time" seconds
      (if record?
          (if (not applied?)
              (begin
                ;(display (seconds->date (current-seconds))) (newline)
                (central-unit 'for-each-steward (lambda(s) ((s 'get-rule-manager) 'execute)))
                (set! applied? #t)
                ;(display "finished executing") (newline)
                )
              'wait)
          (set! applied? #f))
      (loop applied?)))
  (loop #f))
(thread apply-rules)