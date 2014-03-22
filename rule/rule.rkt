#lang racket
(require racket/date)

(provide new-rule)

(define (new-rule element-type value time-int)
  (define (execute steward)
    (cond
      ((not (time-int 'is-in-interval (seconds->date (current-seconds)))) #f) ;not in time-interval
      ((eq? value (steward 'get element-type)) #f) ;condition already true
      (else (steward 'set element-type value))))
  
  (define (dispatch message . args)
    (case message
      ((execute) (apply execute args))))
  
  dispatch)