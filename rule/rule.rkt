#lang r5rs
(#%require racket/date)
(#%require (only racket/base seconds->date))
(#%require (only racket/base current-seconds))

(#%provide new-rule)

(define (new-rule element-type value time-int)
  (define (get-element-type) element-type)
  (define (get-value) value)
  (define (get-interval) time-int)
  (define (execute steward)
    (cond
      ((not (time-int 'is-on-time (seconds->date (current-seconds)))) #f) ;not in time-interval
      ((eq? value (steward 'get element-type)) #f) ;condition already true
      (else (steward 'set element-type value))))
  
  (define (dispatch message . args)
    (case message
      ((get-element-type) (get-element-type))
      ((get-value) (get-value))
      ((get-interval) (get-interval))
      ((execute) (apply execute args))))
  
  dispatch)