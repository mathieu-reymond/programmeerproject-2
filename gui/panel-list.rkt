#lang racket
(require racket/gui/base)

(provide panel-list)

;a list of all the elements of the given list.
;clickable if a callback is given,
;non-clickable otherwise.
;@param prnt this panel's parent
;@param lst a list of strings that will be displayed
;@param cllbck optional callback function for the list-elements
(define (panel-list prnt lst . cllbck)
  (let ((panel (new vertical-panel% [parent prnt])))
    (define (name list-element)
      (if (empty? cllbck)
          (new message% 
               [parent panel]
               [label list-element])
          (new button% 
               [parent panel]
               [label list-element]
               [callback (car cllbck)])))
    (for-each name lst)
    
    panel))