#lang racket
(require racket/date)
(require racket/file)

(provide new-action)
(provide read-log)

(define filepath "C:/Users/Mathieu/Documents/Projects/Scheme/domotica/communication/log.txt")

(define (new-action message)
  (let ((time (current-date)))
    (define (action-to-string)
      (string-append (date->string time #t) " : " message "\n"))
    (define (write)
      (display-to-file (action-to-string) filepath #:exists 'append))
    
    (define (dispatch message . args)
      (case message
        ((to-string) (action-to-string))
        ((write) (write))))
    
    dispatch))

(define (read-log)
  (file->string filepath))