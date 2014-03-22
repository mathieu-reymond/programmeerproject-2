#lang racket
(require racket/gui/base)

(require "../communication/action.rkt")
(provide panel-log)

(define (panel-log prnt)
  (let ((panel (new text-field% 
                    [label "Log"] 
                    [parent prnt]
                    [style (list 'multiple 'hscroll)])))
    (send  panel set-value (read-log))
    panel))