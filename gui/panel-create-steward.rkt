#lang racket
(require racket/gui/base)

(require "../internal/device.rkt"
         "../internal/steward.rkt"
         "../physical/physical-room.rkt"
         "../physical/hardware-device.rkt"
         "../db/manager.rkt")
(provide panel-create-steward)

(define db-path "db/domotica.db")

(define (dialog-serial-numbers prnt steward device-list central-unit)
  (let* ((dialog (new dialog% [label "Serial Numbers"]))
         (panel (new vertical-panel% [parent dialog])))
    (define (add-text-field device)
      (new text-field% [parent panel] [label (device 'get-name)]))
    (define (create button event)
      (let ((room (new-physical-room (steward 'get-room)))) ;temp if add device to steward,  need to extract room
        (for-each (lambda(tf)
                    ;add device to steward
                    (steward 'add-device (new-device (send tf get-label) (send tf get-value)))
                    ;add hardware
                    (new-hardware-device (send tf get-value) room))
                  (send panel get-children))
        ;add to database
        ((new-db-manager db-path) 'add-steward steward)
        ;add to central-unit
        (central-unit 'add-steward steward)))
    (for-each add-text-field device-list)
    (new button% [label "Create"] [parent dialog] [callback create])
    dialog))

(define (list-box-device prnt)
  (let ((list-box (new list-box% [label "Device"] [parent prnt] [choices '()] [style '(vertical-label multiple)])))
    (define (add-device device)
      (send list-box append (device 'get-name) device))
    (device-types 'for-each-device-type add-device)
    list-box))

(define (panel-create-steward prnt central-unit)
  (let* ((panel (new vertical-panel% [parent prnt]))
         (text-field-room (new text-field% [parent panel] [label "Steward Room"]))
         (text-field-ip (new text-field% [parent panel] [label "Steward IP"]))
         (list-device (list-box-device panel)))
    (define (selected-devices)
      (let ((list '()))
        (for-each (lambda(index) (set! list (cons (send list-device get-data index) list))) (send list-device get-selections))
        list))
    (define (create button event)
      (if (central-unit 'get-steward (send text-field-room get-value))
          #f ;display message : already a steward in this room
          (let ((dialog-serial (dialog-serial-numbers panel 
                                                      (new-steward (send text-field-room get-value)
                                                                   (send text-field-ip get-value)) 
                                                      (selected-devices)
                                                      central-unit)))
            (send dialog-serial show #t))))
    (new button% [parent panel] [label "Create"] [callback create])
    panel))