#lang racket
(require racket/date)

(provide new-time-interval)

(define (new-time-interval begin-date end-date recurr)
  (define (get-begin-date) begin-date)
  (define (get-end-date) end-date)
  (define (get-recurrence) recurr)
  (define (is-in-interval date)
    (let loop ((beg begin-date) (end end-date))
      (let ((next (recurr 'next beg end)))
        (cond
          ((< (date->seconds end) (date->seconds date))
           (if next
               (loop (next 'get-begin-date) (next 'get-end-date))
               #f))
          ((< (date->seconds beg) (date->seconds date)) #t)
          (else #f)))))
  
  (define (dispatch message . args)
    (case message
      ((get-begin-date) (get-begin-date))
      ((get-end-date) (get-end-date))
      ((get-recurrence) (get-recurrence))
      ((is-in-interval) (apply is-in-interval args))))
  dispatch)