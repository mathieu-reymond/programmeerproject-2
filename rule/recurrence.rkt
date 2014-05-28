#lang r5rs
(#%require racket/date)
;(#%require (only racket/base error))
(#%require racket/base)

(#%require "time-interval.rkt")
(#%provide new-recurrence)

;b===c* rules/recurrence
; NAME
;  recurrence
; DESCRIPTION
;  Deze klasse zorgt ervoor dat een bepaalde rule herhaald kan worden.
;  De beschikbare herhalingen zijn :
;  * "once"
;  * "daily"
;  * "weekly"
;e===
;until is (day-limit+1 at midnight)-1sec
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
;b===o* recurrence/new-recurrence
; NAME
;  new-recurrence
; DESCRIPTION
;  Maakt een nieuw recurrence object aan.
;  Het type en next-time bepalen het interval tussen een datum
;  en zijn volgende herhaling.
;
;  De beschikare types zijn : 
;  * "once"
;  * "daily"
;  * "weekly"
;
;  Er is ook een einddatum die het einde van herhalingen bepaald.
; PARAMETERS
;  * type - het type van de recurrence.
;  * next-time - het aantal seconden tussen een datum en zijn herhaling.
;  * until - het einddatum van de recurrence.
; SYNOPSIS
(define (new-rec type next-time until)
;e===
  ;b===m* recurrence/get-type
  ; NAME
  ;  get-type
  ; DESCRIPTION
  ;  Geeft het type van deze recurrence terug.
  ; RETURN VALUE
  ;  string - het type van deze recurrence.
  ; SYNOPSIS
  (define (get-type)
  ; SOURCE
    type)
  ;e===
  ;b===m* recurrence/get-end
  ; NAME
  ;  get-end
  ; DESCRIPTION
  ;  Geeft het einddatum van deze recurrence terug.
  ; RETURN VALUE
  ;  date - het einddatum van deze recurrence.
  ; SYNOPSIS
  (define (get-end)
  ; SOURCE
    until)
  ;e===
  ;b===m* recurrence/next
  ; NAME
  ;  next
  ; DESCRIPTION
  ;  Geeft de volgende herhaling van een datum terug.
  ; PARAMETERS
  ;  * date - de datum waarvan de herhaling bekend wilt zijn.
  ; RETURN VALUE
  ;  time-interval - een time-interval met de volgende herhaling en hetzelfde type recurrence.
  ;  #f - false wanneer het einddatum bereikt is.
  ; SYNOPSIS
  (define (next date)
  ; SOURCE
    (let* ((time (date->seconds date))
           (new-time (+ time next-time)))
      (if (< (date->seconds until) new-time)
          #f
          ;(begin (display "made new interval") (newline)
          (new-time-interval (seconds->date new-time) (new-rec type next-time until)))))
  ;e===
  
  (define (dispatch message . args)
    (case message
      ((get-type) (get-type))
      ((get-end) (get-end))
      ((next) (apply next args))))
  
  dispatch)

;no recurrence
(define (new-once until) (new-rec "once" +inf.0 (date->until-date until)))
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