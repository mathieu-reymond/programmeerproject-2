#lang racket
(require racket/gui/base)
(require racket/date)

(provide panel-show-rules)

(define (panel-show-rules prnt central-unit)
  (let* ((panel (new vertical-panel% [parent prnt]))
         (stewards '()) ;choice%
         (rules '())) ;horizontal-panel%
    (define (display-rule rule)
      (define (delete-callback button event)
        (((central-unit 'get-steward (send stewards get-string-selection)) 
          'get-rule-manager) 
         'remove-rule rule))
      (let ((rule-panel (new horizontal-panel% [parent rules])))
        (new message% [parent rule-panel] [label (rule 'to-string)])
        (new button% [parent rule-panel] [label "Delete"] [callback delete-callback])
        rule-panel))
    (define (stewards-callback choice event)
      (for-each (lambda(c) (send rules delete-child c)) (send rules get-children))
      (let loop ((current (((central-unit 'get-steward (send choice get-string-selection)) 
                            'get-rule-manager)
                           'get-rules)))
        (unless (eq? '() current)
          (display-rule (mcar current))
          (loop (mcdr current)))))
    (set! stewards (new choice%
                        [label "Steward"]
                        [parent panel]
                        [choices (let loop ((res '())
                                            (curr (central-unit 'get-stewards)))
                                   (if (eq? '() curr)
                                       res
                                       (loop (cons ((mcar curr) 'get-room) res) (mcdr curr))))]
                        [callback stewards-callback]))
    (set! rules  (new vertical-panel% [parent panel]))
    (if (eq? '() (central-unit 'get-stewards))
        (new message% [parent panel] [label "No stewards"])
        (stewards-callback stewards 'initialize))
    panel))



