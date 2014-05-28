#lang r5rs
(#%require racket/date
           (only racket/base seconds->date)
           (only racket/base current-seconds)
           (prefix et: "../internal/element-type.rkt"))

(#%provide new-rule)

;b===c* rules/rule
; NAME
;  rule
; DESCRIPTION
;  Deze klasse laat toe om automatisch element-types aan te passen
;  op een specifiek tijdstip.
;e===
;b===o* rule/new-rule
; NAME
;  new-rule
; DESCRIPTION
;  Maakt een nieuwe rule aan. 
;  Deze zet een element-type op een nieuwe waarde via execute.
;  De rule kan alleen uitgevoerd worden wanneer de huidige datum deel is van
;  zijn time-interval.
; PARAMETERS
;  * element-type - het element-type die aangepast gaat worden.
;  * value - de waarde waarop het element-type gezet moet worden.
;  * tim-int - het time-interval van deze rule.
; SYNOPSIS
(define (new-rule element-type value time-int)
;e===
  ;b===m* rule/get-element-type
  ; NAME
  ;  get-element-type
  ; DESCRIPTION
  ;  Geeft het element-type van deze rule terug.
  ; RETURN VALUE
  ;  element-type - het element-type van de rule.
  ; SYNOPSIS
  (define (get-element-type)
  ; SOURCE
    element-type)
  ;e===
  ;b===m* rule/get-value
  ; NAME
  ;  get-value
  ; DESCRIPTION
  ;  Geeft de waarde van deze rule terug.
  ; RETURN VALUE
  ;  integer - de waarde van de rule.
  ; SYNOPSIS
  (define (get-value)
  ; SOURCE
    value)
  ;e===
  ;b===m* rule/get-interval
  ; NAME
  ;  get-interval
  ; DESCRIPTION
  ;  Geeft het time-interval van deze rule terug.
  ; RETURN VALUE
  ;  time-interval - het time-interval van de rule.
  ; SYNOPSIS
  (define (get-interval)
  ; SOURCE
    time-int)
  ;e===
  ;b===m* rule/to-string
  ; NAME
  ;  to-string
  ; DESCRIPTION
  ;  Zet de rule om naar een leesbare string.
  ; RETURN VALUE
  ;  string - een string-versie van de rule
  ; SYNOPSIS
  (define (to-string)
  ; SOURCE
    (string-append "Set "
                   (et:to-string element-type) " to "
                   (number->string value) " on "
                   (date->string (time-int 'get-date) #t)
                   (if (eq? (((get-interval) 'get-recurrence) 'get-type) "once")
                       "."
                       (string-append " "
                                      (((get-interval) 'get-recurrence) 'get-type)
                                      " until "
                                      (date->string (((get-interval) 'get-recurrence) 'get-end)) "."))))
  ;e===
  ;b===m* rule/execute
  ; NAME
  ;  execute
  ; DESCRIPTION
  ;  Probeert de rule uit te voeren. 
  ;  Deze wordt alleen uitgevoerd wanneer de huidige datum deelmaakt van het
  ;  time-interval.
  ; PARAMETERS
  ;  * steward - de steward waarop de rule uitgevoerd moet worden.
  ; RETURN VALUE
  ;  #f - false wanneer de rule niet uitgevoerd werd.
  ;  #t - true bij het succesvol uitvoeren van de rule.
  ; SYNOPSIS
  (define (execute steward)
  ; SOURCE
    ;(display "executing rule(") (display (steward 'get-room)) (display ") : ") (display (to-string)) (newline)
    ;(display "time ?") (display (seconds->date (current-seconds))) (newline)
    (cond
      ((not (time-int 'is-on-time (seconds->date (current-seconds))))
       ;(display "not on time") (newline)
       #f) ;not in time-interval
      ((eq? value (steward 'get element-type))
       ;(display "already value") (newline)
       #f) ;condition already true
      (else
       ;(display "applying") (newline)
       (steward 'set element-type value)))
    ;(display "executed rule(") (display (steward 'get-room)) (display ") : ") (display (to-string)) (newline))
    )
  ;e===
  
  (define (dispatch message . args)
    (case message
      ((to-string) (to-string))
      ((get-element-type) (get-element-type))
      ((get-value) (get-value))
      ((get-interval) (get-interval))
      ((execute) (apply execute args))))
  
  dispatch)