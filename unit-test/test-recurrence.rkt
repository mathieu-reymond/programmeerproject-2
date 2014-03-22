#lang r5rs
(#%require rackunit)
(#%require (only racket/base seconds->date))
(#%require racket/date)

(#%require "../rule/recurrence.rkt")
(#%require "../rule/time-interval.rkt")
(#%provide test-recurrence)

;30/1/14 at 10:30
(define start-date (seconds->date (find-seconds 0 30 10 30 1 2014)))
;30/1/14 at 11:30
(define end-date (seconds->date (find-seconds 0 30 11 30 1 2014)))
;7/2/14
(define until (seconds->date (find-seconds 0 30 11 7 2 2014)))
;6/2/14 at 10:30
(define start-weekly-wrong (seconds->date (find-seconds 0 30 10 6 2 2014)))
;6/2/14 at 11:30
(define end-weekly-wrong (seconds->date (find-seconds 0 30 11 6 2 2014)))


(define once (new-once until))
(define daily (new-daily until))
(define weekly (new-weekly until))

(define test-recurrence (lambda () (test-case
                                    "TEST:recurrence.rkt"
                                    (check-equal? ((once 'next start-date end-date) 'get-begin-date)
                                                  start-date
                                                  "method(once)")
                                    (check-equal? ((daily 'next start-date end-date) 'get-begin-date)
                                                  (seconds->date (find-seconds 0 30 10 31 1 2014)) 
                                                  "method(daily)")
                                    (check-equal? ((weekly 'next start-date end-date) 'get-begin-date)
                                                  start-weekly-wrong
                                                  "method(daily) before until")
                                    (check-equal? (weekly 'next start-weekly-wrong end-weekly-wrong)
                                                  #f
                                                  "method(daily) before until")
                                    )))
