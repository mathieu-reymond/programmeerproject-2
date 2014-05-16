#lang r5rs

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

(define (zigbee-string-to-instruction zigbee-string)
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
           ((or (eq? current (string-length string))
                (eq? #\space (string-ref string current)))
            (loop previous current (+ pos 1))) ;no ack/nack
           ((eq? #\: (string-ref string current))
            (vector-set! parsed pos (substring string previous current))
            (loop (+ current 2) (+ current 2) (+ pos 1))) ;current + 2 because skip ": "
           (else (loop previous (+ current 1) pos))))
        ((eq? pos 1)
         (cond
           ((eq? current (string-length string))
            (vector-set! parsed pos (substring string previous current))
            (loop current current (vector-length parsed))) ;finished string
           ((eq? (string-ref string current) #\space)
            (vector-set! parsed pos (substring string previous current))
            (loop (+ current 1) (+ current 1) (+ pos 1))) ;current + 1 because skip " "
           (else
            (loop previous (+ current 1) pos))))
        ((eq? pos 2)
         (cond
           ((eq? current (string-length string))
            (vector-set! parsed pos (substring string previous current))
            (loop current current (vector-length parsed))) ;finished string
           ((eq? (string-ref string current) #\=)
            (vector-set! parsed pos (substring string previous current))
            (loop (+ current 1) (+ current 1) (+ pos 1))) ;current + 1 because skip "="
           (else
            (loop previous (+ current 1) pos))))
        ((eq? pos 3)
         (cond
           ((eq? current (string-length string))
            (vector-set! parsed pos (cons (substring string previous current) (vector-ref parsed pos)))
            (loop current current (vector-length parsed))) ;finished string
           ((eq? (string-ref string current) #\,)
            (vector-set! parsed pos (cons (substring string previous current) (vector-ref parsed pos)))
            (loop (+ current 1) (+ current 1) pos)) ;current + 1 because skip ","
           (else (loop previous (+ current 1) pos))))
        (else ;finished string
         parsed)))
    (loop 0 0 0)))

;used for simulation only
(define (execute-zigbee-string zigbee-string phys-room)
  (let* ((inst-type (substring zigbee-string 0 3))
         (inst (cond
                 ((eq? inst-type "GET") 'get)
                 ((eq? inst-type "SET") 'set)
                 (else #f)))
         (el-type (substring zigbee-string 4 7))
         (el (cond
               ((eq? el-type "POW") 0) ;LIGHT
               ((eq? el-type "TEM") 1) ;TEMPERATURE
               (else #f))))
    (if (eq? inst 'set)
        (let* ((val (substring zigbee-string 8 (string-length zigbee-string)))
               (value (cond
                        ((eq? val "ON") 1)
                        ((eq? val "OFF") 0)
                        (else (string->number val)))))
          (phys-room inst el value))
        (phys-room inst el))))

