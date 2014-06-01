#lang racket
(require racket/gui/base)

(require "panel-create-steward.rkt"
         "panel-modify-steward.rkt")
(provide panel-settings-steward)

(define (panel-settings-steward prnt central-unit)
  (define (change-tab tab event)
    (send tab delete-child (car (send tab get-children)))
    (cond
      ((equal? (send tab get-selection) 0)
       (panel-create-steward tab central-unit))
      ((equal? (send tab get-selection) 1)
       (if (eq? '() (central-unit 'get-stewards))
           (new vertical-panel% [parent tab])
           (panel-modify-steward tab central-unit)))))
  (let ((panel (new tab-panel% 
                    [choices '("New Steward" "Modify Steward")] 
                    [parent prnt] 
                    [callback change-tab])))
    (panel-create-steward panel central-unit)
    panel))