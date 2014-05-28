#lang r5rs
(#%require rackunit)
(#%require (only racket/base seconds->date))
(#%require racket/date)

(#%require "../rule/time-interval.rkt")
(#%require "../rule/recurrence.rkt")
(#%provide test-time-interval)

;30/1/14 at 10:30
(define start-date (seconds->date (find-seconds 0 30 10 30 1 2014)))
;7/2/14
(define until (seconds->date (find-seconds 0 30 11 7 2 2014)))
;6/2/14 at 10:30
(define date-true (seconds->date (find-seconds 0 30 10 6 2 2014)))
;6/2/14 at 12:00
(define date-false-1 (seconds->date (find-seconds 0 00 12 6 2 2014)))
;5/2/14 at 10:30
(define date-false-2 (seconds->date (find-seconds 0 30 10 5 2 2014)))

(define weekly (new-recurrence "weekly" until))

(define time-interval (new-time-interval start-date weekly))

(define test-time-interval (lambda () (test-case
                                       "TEST:time-interval.rkt"
                                       (check-equal? (time-interval 'get-date)
                                                     start-date
                                                     "method(get-date)")
                                       (check-equal? (time-interval 'get-recurrence)
                                                     weekly
                                                     "method(get-recurrence)")
                                       (check-equal? (time-interval 'is-on-time date-true)
                                                     #t
                                                     "method(is-in-interval) in interval after 1 recurrence")
                                       (check-equal? (time-interval 'is-on-time date-false-1)
                                                     #f
                                                     "method(is-in-interval) not in interval but on right day")
                                       (check-equal? (time-interval 'is-on-time date-false-2)
                                                     #f
                                                     "method(is-in-interval) in interval but on wrong day")
                                       )))