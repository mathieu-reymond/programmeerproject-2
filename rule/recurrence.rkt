#lang r5rs
(#%require racket/date)
;(#%require (only racket/base error))
(#%require racket/base)

(#%require "time-interval.rkt")
(#%provide new-recurrence)

(define (date->until-date date)
  (make-date 59
             59
             23
             (date-day date)
             (date-month date)
             (date-year date)
             (date-week-day date)
             (date-year-day date)
             (date-dst? date)
             (date-time-zone-offset date)))

;until is (day-limit+1 at midnight)-1sec
(define (new-rec type next-time until)
  (define (get-type) type)
  (define (get-end) until)
  (define (next date)
    (let* ((time (date->seconds date))
           (new-time (seconds->date (+ time next-time))))
      (if (< (date->seconds until) (date->seconds new-time))
          #f
          (new-time-interval new-time (new-rec type next-time until)))))
  
  (define (dispatch message . args)
    (case message
      ((get-type) (get-type))
      ((get-end) (get-end))
      ((next) (apply next args))))
  
  dispatch)

;no recurrence
(define (new-once until) (new-rec "once" 0 (date->until-date until)))
;every day
(define day 86400)
(define (new-daily until) (new-rec "daily" day (date->until-date until)))
;every week
(define week 604800)
(define (new-weekly until) (new-rec "weekly" week (date->until-date until)))

(define (new-recurrence type until)
  (cond
    ((equal? type "once") (new-once until))
    ((equal? type "daily") (new-daily until))
    ((equal? type "weekly") (new-weekly until))
    (else (error "Error : Recurrence.class : unknown recurrence : " type))))