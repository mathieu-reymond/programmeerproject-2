#lang racket
(require racket/gui/base)
(require racket/date)

(require "../structure/map.rkt")
(require "../internal/element-type.rkt")
(require "../rule/rule.rkt")
(require "../rule/time-interval.rkt")
(require "../rule/recurrence.rkt")
(require "../db/manager.rkt")
(provide panel-new-rule)

(define db-path "db/domotica.db")

(define (numbered-list start end)
  (let loop ((ctr end)
             (res '()))
    (if (< ctr start) 
        res
        (loop (- ctr 1) (cons (number->string ctr) res)))))

(define actuator-map (new-map))
(for-each-element-type (lambda(e) (actuator-map 'add! 
                                                (to-string e)
                                                e)))
(define actuator-list (let loop ((curr (actuator-map 'get-keys))
                                 (res '()))
                        (if (eq? '() curr)
                            res
                            (loop (mcdr curr) (cons (mcar curr) res)))))

(define (panel-new-rule prnt central-unit)
  (let* ((panel (new vertical-panel% [parent prnt]))
         (begin-date (new horizontal-panel% [parent panel]))
         (begin-months (new choice% 
                            [label "Month"] 
                            [choices (numbered-list 1 12)] 
                            [parent begin-date]
                            [selection (- (date-month (current-date)) 1)]))
         (begin-days (new choice% 
                          [label "Day"] 
                          [choices (numbered-list 1 31)] 
                          [parent begin-date]
                          [selection (- (date-day (current-date)) 1)]))
         (begin-hours (new choice% 
                           [label "Hour"] 
                           [choices (numbered-list 0 23)] 
                           [parent begin-date]
                           [selection (- (date-hour (current-date)) 1)]))
         (begin-minutes (new choice% 
                             [label "Minute"] 
                             [choices (numbered-list 0 59)] 
                             [parent begin-date]
                             [selection (- (date-minute (current-date)) 1)]))
         (end-date (new horizontal-panel% [parent panel]))
         (end-months (new choice% 
                          [label "Month"] 
                          [choices (numbered-list 1 12)] 
                          [parent end-date]
                          [selection (- (date-month (current-date)) 1)]))
         (end-days (new choice% 
                        [label "Day"] 
                        [choices (numbered-list 1 31)] 
                        [parent end-date]
                        [selection (- (date-day (current-date)) 1)]))
         (rec-panel (new horizontal-panel% [parent panel]))
         (rec-choice (new choice%
                          [label #f]
                          [choices (list "daily" "weekly")]
                          [parent rec-panel]))
         (recurrence (new check-box%
                          [label "Recurrence"]
                          [parent rec-panel]
                          [callback (lambda(c e) 
                                      (send end-date show (send c get-value))
                                      (send rec-choice show (send c get-value)))]))
         (stewards (new choice%
                        [label "Steward"]
                        [choices (let loop ((res '())
                                            (curr (central-unit 'get-stewards)))
                                   (if (eq? '() curr)
                                       res
                                       (loop (cons ((mcar curr) 'get-room) res) (mcdr curr))))]
                        [parent panel]))
         (element-panel (new horizontal-panel% [parent panel]))
         (element-types (new choice%
                             [label "Element"]
                             [choices actuator-list]
                             [parent element-panel]))
         (new-value (new text-field%
                         [label "Value"]
                         [parent element-panel])))
    (define (rule-callback button event)
      (let ((selected-recurrence (if (send recurrence get-value)
                                     (send rec-choice get-string-selection)
                                     "once"))
            (selected-steward (central-unit 'get-steward (send stewards get-string-selection)))
            (start (seconds->date (find-seconds 0
                                                (string->number (send begin-minutes get-string-selection))
                                                (string->number (send begin-hours get-string-selection))
                                                (string->number (send begin-days get-string-selection))
                                                (string->number (send begin-months get-string-selection))
                                                (date-year (current-date)))))
            (end (seconds->date (find-seconds 0
                                              0
                                              0
                                              (string->number (send end-days get-string-selection))
                                              (string->number (send end-months get-string-selection))
                                              (date-year (current-date))))))
        ((selected-steward 'get-rule-manager) 'add-rule
                                              (new-rule (actuator-map 'find (send element-types get-string-selection))
                                                        (string->number (send new-value get-value))
                                                        (new-time-interval start (new-recurrence selected-recurrence end))))
        ((new-db-manager db-path) 'update-rules selected-steward)))
    (send end-date show #f)
    (send rec-choice show #f)
    (new button% [label "Add Rule"] [parent panel] [callback rule-callback])
    panel))
