#lang racket
(require racket/gui/base)

(require "panel-show-rules.rkt"
         "panel-new-rule.rkt")

(provide panel-rules)

(define (panel-rules prnt central-unit)
  (define (change-tab tab event)
    (send tab delete-child (car (send tab get-children)))
    (cond
      ((equal? (send tab get-selection) 0)
       (panel-show-rules tab central-unit))
      ((equal? (send tab get-selection) 1)
       (panel-new-rule tab central-unit))))
  (let ((panel (new tab-panel% 
                    [choices '("Existing Rules" "New Rule")] 
                    [parent prnt] 
                    [callback change-tab])))
    (panel-show-rules panel central-unit)
    panel))