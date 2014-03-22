#lang racket
(require racket/date)

(require "time-interval.rkt")
(provide new-once)
(provide new-daily)
(provide new-weekly)

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
(define (new-recurrence next-time until)
  (define (next begin-date end-date)
    (let* ((begin-sec (date->seconds begin-date))
           (end-sec (date->seconds end-date))
           (new-begin (seconds->date (+ begin-sec next-time)))
           (new-end (seconds->date (+ end-sec next-time))))
      (if (< (date->seconds until) (date->seconds new-begin))
          #f
          (new-time-interval new-begin new-end (new-recurrence next-time until)))))
  
  (define (dispatch message . args)
    (case message
      ((next) (apply next args))))
  
  dispatch)

;no recurrence
(define (new-once until) (new-recurrence 0 (date->until-date until)))
;every day
(define day 86400)
(define (new-daily until) (new-recurrence day (date->until-date until)))
;every week
(define week 604800)
(define (new-weekly until) (new-recurrence week (date->until-date until)))