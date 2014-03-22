#lang racket
(require racket/gui/base)

(provide panel-modify-steward)

(define (panel-modify-steward prnt central-unit)
  ;panel with device-name and button to remove it from steward
  ;TODO callback
  (define (panel-device prnt device)
    (let ((panel (new horizontal-panel% [parent prnt])))
      (new message% [label (device 'get-name)] [parent panel])
      (new button% [label "remove"] [parent panel])))
  ;choose a steward and show all devices (with button to remove them)
  ;possibility to entirely remove a steward
  (let* ((panel (new vertical-panel% [parent prnt]))
         (stewards (let loop ((current (central-unit 'get-stewards))
                              (res '()))
                     (if (eq? current '())
                         res
                         (loop (mcdr current) (cons ((mcar current) 'get-room) res))))) ;all stewards
         (panel-steward-details (new vertical-panel% [parent panel]))) ;detail panel of selected steward
    (define (stewards-callback choice event)
      (let ((chosen-steward (central-unit 'get-steward
                                          (send choice 
                                                get-string-selection))) ;selected steward
            (panel-steward (new vertical-panel% [parent panel]))) ;new steward's detail panel
        (send panel delete-child panel-steward-details)
        (set! panel-steward-details panel-steward)
        (let loop ((current (chosen-steward 'get-devices)))
          (unless (eq? empty current)
            (panel-device panel-steward (mcar current))
            (loop (mcdr current))))
        (new button% [label "Delete Steward"] [parent panel-steward]) ;TODO callback
        ))
    (new choice% 
         [label "Stewards"]
         [choices stewards]
         [parent panel]
         [callback stewards-callback]))) ;choice box of stewards
  