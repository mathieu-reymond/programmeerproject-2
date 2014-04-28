#lang r5rs
(#%require racket/date
           (only racket/base seconds->date)
           (only racket/base current-seconds)
           (prefix et: "../internal/element-type.rkt"))

(#%provide new-rule)

(define (new-rule element-type value time-int)
  (define (get-element-type) element-type)
  (define (get-value) value)
  (define (get-interval) time-int)
  (define (to-string)
    (string-append "Set "
                   (et:to-string element-type) " to "
                   (number->string value) " on "
                   (date->string (time-int 'get-date))
                   (if (eq? (((get-interval) 'get-recurrence) 'get-type) "once")
                       "."
                       (string-append " "
                                      (((get-interval) 'get-recurrence) 'get-type)
                                      " until "
                                      (date->string (((get-interval) 'get-recurrence) 'get-end)) "."))))
  (define (execute steward)
    (cond
      ((not (time-int 'is-on-time (seconds->date (current-seconds)))) #f) ;not in time-interval
      ((eq? value (steward 'get element-type)) #f) ;condition already true
      (else (steward 'set element-type value))))
  
  (define (dispatch message . args)
    (case message
      ((to-string) (to-string))
      ((get-element-type) (get-element-type))
      ((get-value) (get-value))
      ((get-interval) (get-interval))
      ((execute) (apply execute args))))
  
  dispatch)