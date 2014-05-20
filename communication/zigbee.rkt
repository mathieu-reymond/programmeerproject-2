#lang r5rs

(#%require "../internal/element-type.rkt"
           "../structure/map.rkt")
(#%provide zigbee-transmit-request
           zigbee-transmit-status
           zigbee-recieve-paquet
           new-zigbee-instruction
           new-zigbee-message
           zigbee-message-to-zigbee-instruction
           zigbee-instruction-to-zigbee-string
           execute-zigbee-string)

(define zigbee-transmit-request 16) ;0x10
(define zigbee-transmit-status 139) ;0x8b
(define zigbee-recieve-paquet 144) ;0x90
(define line-feed 10)

(define (new-zigbee-instruction zigbee-string)
  (let ((vector (make-vector (string-length zigbee-string))))
    (define (loop current)
      (cond
        ((eq? current (string-length zigbee-string)) vector)
        (else (vector-set! vector current (char->integer (string-ref zigbee-string current)))
              (loop (+ current 1)))))
    (loop 0)))

(define (new-zigbee-message type address64 address16 zigbee-instruction)
  (let ((current 1)
        (message (make-vector (+ 1 ;type
                                 8 ;64bit address
                                 3 ;16bit address
                                 (vector-length zigbee-instruction)
                                 2)))) ;2 line feeds
    (define (loop vector i)
      (cond
        ((eq? i (vector-length vector)) message)
        (else
         (vector-set! message current (vector-ref vector i))
         (set! current (+ current 1))
         (loop vector (+ i 1)))))
    (vector-set! message 0 type)
    (loop address64 0)
    (loop address16 0)
    (loop zigbee-instruction 0)
    (loop (vector line-feed line-feed) 0)
    message))

(define (zigbee-message-to-zigbee-instruction zigbee-message)
  (let ((vector (make-vector (- (vector-length zigbee-message) 14))))
    (define (copy from to length)
      (cond
        ((eq? length 0) vector)
        (else
         (vector-set! vector to (vector-ref zigbee-message from))
         (copy (+ from 1) (+ to 1) (- length 1)))))
    (copy 12 0 (vector-length vector))
    vector))

(define (zigbee-instruction-to-zigbee-string zigbee-instruction)
  (let ((vec (make-vector (vector-length zigbee-instruction))))
    (define (to-char current)
      (cond
        ((eq? current (vector-length vec)) #t)
        (else (vector-set! vec current (integer->char (vector-ref zigbee-instruction current)))
              (to-char (+ current 1)))))
    (to-char 0)
    (apply string (vector->list vec))))

(define (split-zigbee-string zigbee-string)
  (define (string-to-values lst)
    (cond
      ((eq? '() lst) #t) ;finished
      (else
       (set-car! lst
                 (cond
                   ((equal? "ON" (car lst)) 1)
                   ((equal? "OFF" (car lst)) 0)
                   (else (string->number (car lst)))))
       (string-to-values (cdr lst)))))
  ;0 : 'ack, 'nack, #f
  ;1 : 'get, 'set
  ;2 : 'pow, 'tem
  ;3 : listof 'on, 'off, integer
  (let ((parsed (make-vector 4 #f)))
    (vector-set! parsed (- (vector-length parsed) 1) '())
    (define (loop previous current pos)
      (cond
        ((eq? pos 0)
         (cond
           ((or (eq? current (string-length zigbee-string))
                (eq? #\space (string-ref zigbee-string current)))
            (loop previous current (+ pos 1))) ;no ack/nack
           ((eq? #\: (string-ref zigbee-string current))
            (vector-set! parsed pos (substring zigbee-string previous current))
            (loop (+ current 2) (+ current 2) (+ pos 1))) ;current + 2 because skip ": "
           (else (loop previous (+ current 1) pos))))
        ((eq? pos 1)
         (cond
           ((eq? current (string-length zigbee-string))
            (vector-set! parsed pos (substring zigbee-string previous current))
            (loop current current (vector-length parsed))) ;finished string
           ((eq? (string-ref zigbee-string current) #\space)
            (vector-set! parsed pos (substring zigbee-string previous current))
            (loop (+ current 1) (+ current 1) (+ pos 1))) ;current + 1 because skip " "
           (else
            (loop previous (+ current 1) pos))))
        ((eq? pos 2)
         (cond
           ((eq? current (string-length zigbee-string))
            (vector-set! parsed pos (substring zigbee-string previous current))
            (loop current current (vector-length parsed))) ;finished string
           ((eq? (string-ref zigbee-string current) #\=)
            (vector-set! parsed pos (substring zigbee-string previous current))
            (loop (+ current 1) (+ current 1) (+ pos 1))) ;current + 1 because skip "="
           (else
            (loop previous (+ current 1) pos))))
        ((eq? pos 3)
         (cond
           ((eq? current (string-length zigbee-string))
            (vector-set! parsed pos (cons (substring zigbee-string previous current) (vector-ref parsed pos)))
            (loop current current (vector-length parsed))) ;finished string
           ((eq? (string-ref zigbee-string current) #\,)
            (vector-set! parsed pos (cons (substring zigbee-string previous current) (vector-ref parsed pos)))
            (loop (+ current 1) (+ current 1) pos)) ;current + 1 because skip ","
           (else (loop previous (+ current 1) pos))))
        (else ;finished string
         (string-to-values (vector-ref parsed 3)) ;convert in readable values
         parsed)))
    (loop 0 0 0)))

;used for simulation only
(define (execute-zigbee-string zigbee-string phys-room)
  (define (find-key-for-element element map)
    (define (loop keys)
      (cond
        ((eq? keys '()) #f) ;no such element in map
        ((equal? (map 'find (car keys)) element)
         (car keys))
        (else (loop (cdr keys)))))
    (loop (map 'get-keys)))
  (let* ((split (split-zigbee-string zigbee-string))
         (el (find-key-for-element (vector-ref split 2) element-type-zigbee-type-map)))
    (cond
      ((equal? (vector-ref split 1) "SET")
       (string-append
        (if (phys-room 'set el (car (vector-ref split 3)))
            "ACK: "
            "NACK: ")
        zigbee-string))
      (else
       (string-append (element-type-zigbee-type-map 'find el)
                      "="
                      (let ((val (phys-room 'get el)))
                        (if (eq? el LIGHT)
                            (if (equal? val 1)
                                "ON"
                                "OFF")
                            (number->string val))))))))

