#lang r5rs
(#%require racket/date)

(#%provide new-time-interval)

(define (new-time-interval date recurr)
  (define (get-date) date)
  (define (get-recurrence) recurr)
  (define (is-on-time time)
    (let loop ((d date))
      (let ((next (recurr 'next d)))
        (cond
          ((< (date->seconds d) (date->seconds time))
           (if next
               (loop (next 'get-date))
               #f))
          ((= (date->seconds d) (date->seconds time)) #t)
          (else #f)))))
  
  (define (dispatch message . args)
    (case message
      ((get-date) (get-date))
      ((get-recurrence) (get-recurrence))
      ((is-on-time) (apply is-on-time args))))
  dispatch)