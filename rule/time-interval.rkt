#lang r5rs
(#%require racket/date)

(#%provide new-time-interval)

;b===c* rules/time-interval
; NAME
;  time-interval
; DESCRIPTION
;  De time-interval klasse bepaalt of een datum
;  deel uitmaakt van de datum (herhalingen inbegrepen) van deze interval.
;e===
;b===o* time-interval/new-time-interval
; NAME
;  new-time-interval
; DESCRIPTION
;  Maakt een nieuwe time-interval object aan.
;  Een time-interval begint op een datum en heeft een
;  type recurrence.
; PARAMETERS
;  date - de begin-datum van dit time-interval.
;  recurr - de recurrence van dit interval.
; SYNOPSIS
(define (new-time-interval date recurr)
;e===
  ;b===m* time-interval/get-date
  ; NAME
  ;  get-date
  ; DESCRIPTION
  ;  Geeft de begindatum van het interval terug.
  ; RETURN VALUE
  ;  date - de begindatum van dit time-interval.
  ; SYNOPSIS
  (define (get-date)
  ; SOURCE
    date)
  ;e===
  ;b===m* time-interval/get-recurrence
  ; NAME
  ;  get-recurrence
  ; DESCRIPTION
  ;  Geeft de recurrence van dit time-interval terug.
  ; RETURN VALUE
  ;  recurrence - de recurrence van dit interval.
  ; SYNOPSIS
  (define (get-recurrence)
  ; SOURCE
    recurr)
  ;e===
  ;b===m* time-interval/is-on-time
  ; NAME
  ;  is-on-time
  ; DESCRIPTION
  ;  Bepaalt of een datum deelmaakt van dit time-interval
  ;  (als het gelijk is aan de begindatum of een van zijn herhalingen).
  ; PARAMETERS
  ;  * time - de datum die vergeleken moet worden.
  ; RETURN VALUE
  ;  #t - true als het deelmaakt van dit interval.
  ;  #f - false als het niet deelmaakt van dit interval.
  ; SYNOPSIS
  (define (is-on-time time)
  ; SOURCE
    (let loop ((d date))
      (let ((next (recurr 'next d)))
        (cond
          ((< (date->seconds d) (date->seconds time))
           (if next
               (loop (next 'get-date))
               #f))
          ((= (date->seconds d) (date->seconds time)) #t)
          (else #f)))))
  ;e===
  
  (define (dispatch message . args)
    (case message
      ((get-date) (get-date))
      ((get-recurrence) (get-recurrence))
      ((is-on-time) (apply is-on-time args))))
  dispatch)