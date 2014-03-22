#lang racket
(require racket/gui/base)

(require "panel-overview-steward.rkt")
(require "panel-history-steward.rkt")

(provide panel-steward)

(define (panel-steward prnt steward)
  (define (change-tab tab event)
    (send tab delete-child (car (send tab get-children)))
    (cond
      ((equal? (send tab get-selection) 0)
       (panel-overview-steward tab steward))
      ((equal? (send tab get-selection) 1)
       (panel-history-steward tab steward))))
  ;(new message% [parent tab] [label "History"]))))
  (let* ((panel (new tab-panel% [choices '("Overview" "History")] [parent prnt] [callback change-tab])))
    (panel-overview-steward panel steward)
    panel))