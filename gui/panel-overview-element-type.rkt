#lang racket
(require racket/gui/base)

(require "../internal/element-type.rkt")

(provide panel-overview-element-type)

(define (panel-overview-element-type prnt element-type central-unit)
  (let* ((panel (new vertical-panel% 
                     [parent prnt]
                     [alignment '(left top)])))
    (new message% [parent panel] [label (to-string element-type)])
    (let* ((hor-panel (new horizontal-panel% [parent panel]))
           (left-panel (new vertical-panel% [parent hor-panel] [alignment '(left top)]))
           (right-panel (new vertical-panel% [parent hor-panel] [alignment '(right top)])))
      (define (steward-name s)
        (new message% [parent left-panel] [label (s 'get-room)]))
      (define (steward-value s)
        (new message% [parent right-panel] [label (if (s 'get element-type) 
                                                      (s 'get element-type) 
                                                      "unknown")]))
      (central-unit 'for-each-steward steward-name)
      (central-unit 'for-each-steward steward-value))
    
    panel))