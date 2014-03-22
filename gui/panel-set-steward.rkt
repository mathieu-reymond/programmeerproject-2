#lang racket
(require racket/gui/base)

(require "../internal/element-type.rkt")
(require "../internal/actuator.rkt")
(provide panel-set-steward)

(define (panel-set-steward prnt steward)
  (define (panel-set-actuator prnt actuator)
    (let ((panel (new horizontal-panel% [parent prnt]))
          (type ((actuator 'super) 'get-type)))
      (define (slider-callback slider event)
        (steward 'set type (send slider get-value)))
      (new slider% 
           [parent panel]
           [label (to-string type)]
           [min-value 0]
           [max-value 100]
           [callback slider-callback])))
  (let ((panel (new vertical-panel% [parent prnt])))
    (let loop ((current (steward 'get-devices)))
      (newline)
      (cond
        ((eq? '() current) #t)
        (else
         (let actuator-loop ((current-act ((mcar current) 'get-elements)))
           (unless (eq? '() current-act)
             (when (eq? ((mcar current-act) 'class) actuator)
               (panel-set-actuator panel (mcar current-act)))
             (actuator-loop (mcdr current-act))))
         (loop (mcdr current)))))
    panel))