#lang racket
(require racket/gui/base)

(require "../internal/central-unit.rkt")
(require "../internal/steward.rkt")
(require "../internal/element-type.rkt")
(require "../internal/sensor.rkt")
(require "../internal/actuator.rkt")
(require "../internal/device.rkt")
(require "../physical/physical-room.rkt")
(require "../physical/hardware-device.rkt")
(require "../db/manager.rkt")
(require "panel-overview-steward.rkt")
(require "panel-overview-element-type.rkt")
(require "panel-history-steward.rkt")
(require "panel-steward.rkt")
(require "panel-log.rkt")
(require "panel-modify-steward.rkt")

(provide main-frame)

(define db-path "db/domotica.db")

(define (panel-steward-list prnt cllbck central-unit)
  (let ((panel (new vertical-panel% [parent prnt])))
    (define (steward-name steward)
      (new button% [parent panel]
           [label (steward 'get-room)]
           [callback cllbck]))
    (central-unit 'for-each-steward steward-name)
    
    panel))

(define (panel-element-type prnt element-type central-unit)
  (define (change-tab tab event)
    (send tab delete-child (car (send tab get-children)))
    (cond
      ((equal? (send tab get-selection) 0)
       (panel-overview-element-type tab element-type central-unit))
      ((equal? (send tab get-selection) 1)
       (new message% [parent tab] [label "History"]))))
  (let* ((panel (new tab-panel% [choices '("Overview" "History")] [parent prnt] [callback change-tab])))
    (panel-overview-element-type panel element-type central-unit)
    panel))

(define (panel-overview-history prnt central-unit)
  (let ((panel (new horizontal-panel% [parent prnt])))
    (define (list-callback button event)
      (let ((steward (central-unit 'get-steward (send button get-label))))
        (send panel delete-child (cadr (send panel get-children))) ;delete second child
        (panel-steward panel steward)))
    (panel-steward-list panel list-callback central-unit)
    (let ((steward #f))
      (central-unit 'for-each-steward (lambda(s) (if (not steward)
                                                     (set! steward s)
                                                     'else)))
      (panel-steward panel steward))
    
    panel))

(define (panel-device-list prnt cllbck)
  (let ((panel (new vertical-panel% [parent prnt])))
    (define (device-name device)
      (new button% [parent panel]
           [label (device 'get-name)]
           [callback cllbck]))
    (device-types 'for-each-device-type device-name)
    
    panel))

(define (list-box-sensor prnt)
  (let ((list-box (new list-box% [label "Sensor"] [parent prnt] [choices '()] [style '(vertical-label multiple)])))
    (define (add-sensor element-type)
      (send list-box append (to-string element-type) (new-sensor element-type)))
    (for-each-element-type add-sensor)
    
    list-box))
(define (list-box-actuator prnt)
  (let ((list-box (new list-box% [label "Actuator"] [parent prnt] [choices '()] [style '(vertical-label multiple)])))
    (define (add-actuator element-type)
      (send list-box append (to-string element-type) (new-actuator element-type)))
    (for-each-element-type add-actuator)
    
    list-box))

(define (panel-new-device prnt)
  (let* ((panel (new vertical-panel% [parent prnt]))
         (text-field-name (new text-field% [parent panel] [label "Device Name"]))
         (list-sensor (list-box-sensor panel))
         (list-actuator (list-box-actuator panel)))
    (define (create button event)
      (if (device-types 'contains? (send text-field-name get-value))
          #f ;display message : device with this name already exists
          (let ((device (new-device (send text-field-name get-value) "default")))
            (define (sensor index)
              (device 'add-element (send list-sensor get-data index)))
            (define (actuator index)
              (device 'add-element (send list-actuator get-data index)))
            (for-each sensor (send list-sensor get-selections))
            (for-each actuator (send list-actuator get-selections))
            ;add to database
            ((new-db-manager db-path) 'add-device-type device)))) ;display message : device added
    (new button% [label "Create"] [parent panel] [callback create])
    panel))

(define (panel-settings-device prnt)
  (let* ((panel (new horizontal-panel% [parent prnt]))
         (device-list (panel-device-list panel (lambda(b e) void)))
         (new-device (panel-new-device panel)))
    panel))

(define (list-box-device prnt)
  (let ((list-box (new list-box% [label "Device"] [parent prnt] [choices '()] [style '(vertical-label multiple)])))
    (define (add-device device)
      (send list-box append (device 'get-name) device))
    (device-types 'for-each-device-type add-device)
    list-box))

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

(define (panel-create-steward prnt central-unit)
  (let* ((panel (new vertical-panel% [parent prnt]))
         (text-field-room (new text-field% [parent panel] [label "Steward Room"]))
         (list-device (list-box-device panel)))
    (define (selected-devices)
      (let ((list '()))
        (for-each (lambda(index) (set! list (cons (send list-device get-data index) list))) (send list-device get-selections))
        list))
    (define (create button event)
      (if (central-unit 'get-steward (send text-field-room get-value))
          #f ;display message : already a steward in this room
          (let ((dialog-serial (dialog-serial-numbers panel 
                                                      (new-steward (send text-field-room get-value)) 
                                                      (selected-devices)
                                                      central-unit)))
            (send dialog-serial show #t))))
    (new button% [parent panel] [label "Create"] [callback create])
    panel))

(define (panel-settings-steward prnt central-unit)
  (let* ((panel (new horizontal-panel% [parent prnt]))
         ;(list-steward (panel-steward-list panel (lambda(b e) void) central-unit))
         (list-steward (panel-modify-steward panel central-unit))
         (new-steward (panel-create-steward panel central-unit)))
    panel))

(define (panel-main prnt central-unit)
  (let* ((panel (new vertical-panel% [parent prnt]))
         (panel-button (new horizontal-panel% [parent panel]))
         (panel-current (panel-overview-history panel central-unit)))
    (define (callback-overview button event)
      (send panel delete-child panel-current)
      (set! panel-current (panel-overview-history panel central-unit)))
    (define (callback-settings-steward button event)
      (send panel delete-child panel-current)
      (set! panel-current (panel-settings-steward panel central-unit)))
    (define (callback-settings-device button event)
      (send panel delete-child panel-current)
      (set! panel-current (panel-settings-device panel)))
    (define (callback-log button event)
      (send panel delete-child panel-current)
      (set! panel-current (panel-log panel)))
    (new button% [parent panel-button] [label "Overview"] [callback callback-overview])
    (new button% [parent panel-button] [label "New Steward"] [callback callback-settings-steward])
    (new button% [parent panel-button] [label "New Device"] [callback callback-settings-device])
    (new button% [parent panel-button] [label "Log"] [callback callback-log])
    panel))

(define (main-frame central-unit m-width m-height)
  (let ((main-frame (new frame% [label "Domotica"] [min-width m-width] [min-height m-height])))
    (panel-main main-frame central-unit)
    main-frame))