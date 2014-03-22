#lang racket
(require racket/gui/base)

(require "../internal/element-type.rkt")
(require "panel-set-steward.rkt")
(provide panel-overview-steward)

(define (panel-overview-steward prnt steward)
  (let* ((panel (new vertical-panel% 
                     [parent prnt]
                     [alignment '(left top)])))
    (new message% 
         [parent panel]
         [label (steward 'get-room)])
    (let* ((hor-panel (new horizontal-panel% [parent panel]))
           (left-panel (new vertical-panel% [parent hor-panel]
                            [alignment '(left top)]))
           (right-panel (new vertical-panel% [parent hor-panel]
                             [alignment '(right top)])))
      (define (element-name e)
        (new message% [parent left-panel] [label (to-string e)]))
      (define (element-value e)
        (new message% [parent right-panel] [label (if (steward 'get e) (number->string (steward 'get e)) "unknown")]))
      (for-each-element-type element-name)
      (for-each-element-type element-value))
    
    (panel-set-steward panel steward)
    
    panel))