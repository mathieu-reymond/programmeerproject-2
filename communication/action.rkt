#lang racket
(require racket/date)
(require racket/file)

(provide new-action)
(provide read-log)

(define filepath "C:/Users/Mathieu/Documents/Projects/Scheme/domotica/communication/log.txt")

;b===c* communication/action
; NAME
;  action
; DESCRIPTION
;  De action classe schrijft messages naar een log file.
;  Zo worden de instructions die een steward sendt in een log-file opgeslagen.
;e===
;b===o* action/new-action
; NAME
;  new-action
; DESCRIPTION
;  Maakt een nieuw action object.
; PARAMETERS
;  message - de string die geschreven moet worden.
; SYNOPSIS
(define (new-action message)
;e===
  (let ((time (current-date)))
    ;b===m* action/to-string
    ; NAME
    ;  to-string
    ; DESCRIPTION
    ;  Een string representatie van het action object.
    ; RETURN VALUE
    ;  string - de representatie van het action object.
    ; SYNOPSIS
    (define (action-to-string)
    ; SOURCE
      (string-append (date->string time #t) " : " message "\n"))
    ;e===
    ;b===m* action/write
    ; NAME
    ;  write
    ; DESCRIPTION
    ;  Schrijft de action in de log file.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (write)
    ; SOURCE
      (display-to-file (action-to-string) filepath #:exists 'append))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((to-string) (action-to-string))
        ((write) (write))))
    
    dispatch))

(define (read-log)
  (file->string filepath))