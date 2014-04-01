#lang racket
(require racket/gui/base)

(require "../db/manager.rkt")
(provide panel-modify-steward)

(define db-path "db/domotica.db")

(define (panel-modify-steward prnt central-unit)
  (define (steward-rooms)
    (let loop ((current (central-unit 'get-stewards))
               (res '()))
      (if (eq? current '())
          res
          (loop (mcdr current) (cons ((mcar current) 'get-room) res)))))
  ;choose a steward and show all devices (with button to remove them)
  ;possibility to entirely remove a steward
  (let* ((panel (new vertical-panel% [parent prnt]))
         (stewards (steward-rooms)) ;all stewards
         (choice-box '()) ;a choice% to select a steward
         (panel-steward-details (new vertical-panel% [parent panel]))) ;detail panel of selected steward
    ;panel with device-name and button to remove it from steward
    (define (panel-device prnt device)
      (define (callback-remove-device button event)
        ((central-unit 'get-steward (send choice-box get-string-selection)) 'remove-device device)
        ((new-db-manager db-path) 'remove-device device)) ;remove from database
      (let ((panel (new horizontal-panel% [parent prnt])))
        (new message% [label (device 'get-name)] [parent panel])
        (new button% [label "remove"] [parent panel] [callback callback-remove-device])))
    ;show devices from selected steward
    (define (stewards-callback choice event)
      (let ((chosen-steward (central-unit 'get-steward
                                          (send choice 
                                                get-string-selection))) ;selected steward
            (panel-steward (new vertical-panel% [parent panel]))) ;new steward's detail panel
        (define (callback-remove-steward button event)
          (central-unit 'remove-steward chosen-steward)
          ((new-db-manager db-path) 'remove-steward chosen-steward)) ;remove from database
        (send panel delete-child panel-steward-details)
        (set! panel-steward-details panel-steward)
        (let loop ((current (chosen-steward 'get-devices)))
          (unless (eq? empty current)
            (panel-device panel-steward (mcar current))
            (loop (mcdr current))))
        (new button% [label "Delete Steward"] [parent panel-steward] [callback callback-remove-steward])
        ))
    (set! choice-box
          (new choice% 
               [label "Stewards"]
               [choices stewards]
               [parent panel]
               [callback stewards-callback])) ;choice box of stewards
    (stewards-callback choice-box 'initialize)
    panel))
