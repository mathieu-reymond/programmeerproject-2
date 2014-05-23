#lang r5rs

(#%require "../internal/element-type.rkt"
           "../structure/map.rkt")
(#%provide zigbee-transmit-request
           zigbee-transmit-status
           zigbee-recieve-paquet
           new-zigbee-instruction
           new-zigbee-message
           zigbee-vector-to-zigbee-string
           execute-zigbee-instruction)

(define zigbee-transmit-request 16) ;0x10
(define zigbee-transmit-status 139) ;0x8b
(define zigbee-recieve-paquet 144) ;0x90
(define line-feed 10)

(define (subvector to-copy from to)
  (let ((vect (make-vector (- to from))))
    (do ((i 0 (+ i 1)))
      ((eq? (+ i from) to) vect)
      (vector-set! vect i (vector-ref to-copy (+ from i))))))

(define (vector-append . args)
  (define (size-loop res current)
    (if (eq? current '())
        res
        (size-loop (+ res (vector-length (car current))) (cdr current))))
  (let ((vect (make-vector (size-loop 0 args))))
    (define (vector-copy from to to-copy)
      (if (eq? from (vector-length to-copy))
          to
          (begin
            (vector-set! vect to (vector-ref to-copy from))
            (vector-copy (+ from 1) (+ to 1) to-copy))))
    (define (copy-loop to current)
      (if (eq? current '())
          vect
          (copy-loop (vector-copy 0 to (car current)) (cdr current))))
    (copy-loop 0 args)))

(define (string-to-value element-type str)
  (cond
    ((equal? LIGHT element-type)
     (if (equal? str "ON")
         1
         0))
    (else (string->number str))))
(define (value-to-string element-type val)
  (cond
    ((equal? LIGHT element-type)
     (if (equal? val 0)
         "OFF"
         "ON"))
    (else (number->string val))))

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
    (vector-set! parsed 2 '())
    (vector-set! parsed (- (vector-length parsed) 1) (new-map))
    (let ((current-element #f)
          (current-values '()))
      (define (loop previous current pos)
        (cond
          ((eq? pos 0)
           (cond
             ((or (eq? current (- (string-length zigbee-string) 1))
                  (eq? #\space (string-ref zigbee-string current)))
              (loop previous current (+ pos 1))) ;no ack/nack
             ((eq? #\: (string-ref zigbee-string current))
              (vector-set! parsed pos (substring zigbee-string previous current))
              (loop (+ current 2) (+ current 2) (+ pos 1))) ;current + 2 because skip ": "
             ((equal? #\= (string-ref zigbee-string current))
              (loop previous current (+ pos 2))) ;no ack/nack, no get/set
             (else (loop previous (+ current 1) pos))))
          ((eq? pos 1)
           (cond
             ((eq? current (- (string-length zigbee-string) 1))
              (vector-set! parsed pos (substring zigbee-string previous current))
              (loop current current (vector-length parsed))) ;finished string
             ((eq? (string-ref zigbee-string current) #\space)
              (vector-set! parsed pos (substring zigbee-string previous current))
              (loop (+ current 1) (+ current 1) (+ pos 1))) ;current + 1 because skip " "
             (else
              (loop previous (+ current 1) pos))))
          ((eq? pos 2)
           (cond
             ((eq? current (- (string-length zigbee-string) 1))
              (set! current-element (substring zigbee-string previous current))
              (set! current-values '())
              (vector-set! parsed pos (cons current-element (vector-ref parsed pos)))
              (loop current current (+ pos 1))) ;finished string
             ((eq? (string-ref zigbee-string current) #\=)
              (set! current-element (substring zigbee-string previous current))
              (set! current-values '())
              (vector-set! parsed pos (cons current-element (vector-ref parsed pos)))
              (loop (+ current 1) (+ current 1) (+ pos 1))) ;current + 1 because skip "="
             (else
              (loop previous (+ current 1) pos))))
          ((eq? pos 3)
           (cond
             ((eq? current (- (string-length zigbee-string) 1))
              (let ((el (substring zigbee-string previous current)))
                ((vector-ref parsed pos) 'add! current-element (if (equal? el "")
                                                                   '()
                                                                   (cons (string-to-value (element-type-zigbee-type-map 'key
                                                                                                                        current-element)
                                                                                          el)
                                                                         current-values))))
              (loop current current (vector-length parsed))) ;finished string
             ((eq? (string-ref zigbee-string current) #\newline)
              ((vector-ref parsed pos) 'add! current-element (cons (string-to-value (element-type-zigbee-type-map 'key
                                                                                                                  current-element)
                                                                                    (substring zigbee-string previous current)) current-values))
              (set! current-element '())
              (loop (+ current 1) (+ current 1) (- pos 1))) ;next element-type (POW, TEM...); +1 because skip newline
             ((eq? (string-ref zigbee-string current) #\,)
              (set! current-values (cons (string-to-value (element-type-zigbee-type-map 'key
                                                                                        current-element)
                                                          (substring zigbee-string previous current)) current-values))
              (loop (+ current 1) (+ current 1) pos)) ;current + 1 because skip ","
             (else (loop previous (+ current 1) pos))))
          (else ;finished string
           ;(string-to-values (vector-ref parsed 3)) ;convert in readable values
           parsed)))
      (loop 0 0 0))))

(define (zigbee-vector-to-zigbee-string zigbee-vector)
  (let ((vec (make-vector (vector-length zigbee-vector))))
    (define (to-char current)
      (cond
        ((eq? current (vector-length vec)) #t)
        (else (vector-set! vec current (integer->char (vector-ref zigbee-vector current)))
              (to-char (+ current 1)))))
    (to-char 0)
    (apply string (vector->list vec))))

(define (new-zigbee-instruction zigbee-string)
  (define (string-downcase str)
    (define (loop current res)
      (if (eq? current -1) 
          (apply string res)
          (loop (- current 1) (cons (char-downcase (string-ref str current)) res))))
    (loop (- (string-length str) 1) '()))
  (define (zigbee-string-to-vector zigbee-string)
    (let ((vector (make-vector (string-length zigbee-string))))
      (define (loop current)
        (cond
          ((eq? current (string-length zigbee-string)) vector)
          (else (vector-set! vector current (char->integer (string-ref zigbee-string current)))
                (loop (+ current 1)))))
      (loop 0)))
  (let ((split (split-zigbee-string zigbee-string))
        (vect (zigbee-string-to-vector zigbee-string)))
    (define (to-string) zigbee-string)
    (define (to-vector) vect)
    (define (acknowledged?) (equal? "ACK" (vector-ref split 0)))
    (define (type) (string->symbol (string-downcase (vector-ref split 1))))
    (define (element-types) (map (lambda(e) (element-type-zigbee-type-map 'key e)) (vector-ref split 2)))
    (define (values element-type) ((vector-ref split 3) 'find (element-type-zigbee-type-map 'find element-type)))
    
    (define (dispatch message . args)
      (case message
        ((to-string) (to-string))
        ((to-vector) (to-vector))
        ((acknowledged?) (acknowledged?))
        ((type) (type))
        ((element-types) (element-types))
        ((values) (apply values args))))
    
    dispatch))

;used for simulation only
(define (execute-zigbee-instruction zigbee-instruction phys-room)
  (let ((args '()))
    (define (add-args add) (set! args (cons add args)))
    (cond
      ((equal? (zigbee-instruction 'type) 'set)
       (let* ((et (car (zigbee-instruction 'element-types)))
              (res (phys-room 'set 
                              et
                              (car (zigbee-instruction 'values et)))))
         (if res
             (add-args "ACK: ")
             (add-args "NACK: "))
         (add-args (zigbee-instruction 'to-string))))
      ((equal? (zigbee-instruction 'type) 'get)
       (for-each (lambda(et) (add-args (element-type-zigbee-type-map 'find et))
                   (add-args "=")
                   (add-args (value-to-string et (phys-room 'get et)))
                   (add-args "\n"))
                 (element-type-zigbee-type-map 'get-keys)))
      (else (add-args "NACK: ")
            (add-args (zigbee-instruction 'to-string))))
    (apply string-append (reverse args))))


(define (new-zigbee-message-x10 id address64 address16 broadcast-radius options zigbee-vector)
  (let ((vect (vector-append (vector zigbee-transmit-request)
                             (vector id)
                             address64
                             address16
                             (vector broadcast-radius)
                             (vector options)
                             zigbee-vector))
        (split (split-zigbee-string (zigbee-vector-to-zigbee-string zigbee-vector))))
    (define (type) zigbee-transmit-request)
    (define (to-vector) vect)
    (define (get-address64) address64)
    (define (get-zigbee-instruction) (new-zigbee-instruction (zigbee-vector-to-zigbee-string zigbee-vector)))
    
    (define (dispatch message . args)
      (case message
        ((type) (type))
        ((to-vector) (to-vector))
        ((get-address64) (get-address64))
        ((get-zigbee-instruction) (get-zigbee-instruction))
        ))
    
    dispatch))

(define (new-zigbee-message-x8b id address16 retry-count delivery-status discovery-status)
  (let ((vect (vector-append (vector zigbee-transmit-status)
                             (vector id)
                             address16 
                             (vector retry-count)
                             (vector delivery-status)
                             (vector discovery-status))))
    (define (type) zigbee-transmit-status)
    (define (to-vector) vect)
    (define (delivered?) (eq? delivery-status 0))
    (define (get-retry-count) retry-count)
    
    (define (dispatch message . args)
      (case message
        ((type) (type))
        ((to-vector) (to-vector))
        ((delivered?) (delivered?))
        ((get-retry-count) (get-retry-count))
        ))
    
    dispatch))

(define (new-zigbee-message-x90 address64 address16 recieve-option zigbee-vector)
  (let ((vect (vector-append (vector zigbee-recieve-paquet)
                             address64
                             address16
                             (vector recieve-option)
                             zigbee-vector
                             (vector line-feed)))
        (split (split-zigbee-string (zigbee-vector-to-zigbee-string zigbee-vector))))
    (define (type) zigbee-recieve-paquet)
    (define (to-vector) vect)
    (define (get-address64) address64)
    (define (get-zigbee-instruction) (new-zigbee-instruction (zigbee-vector-to-zigbee-string zigbee-vector)))
    
    (define (dispatch message . args)
      (case message
        ((type) (type))
        ((to-vector) (to-vector))
        ((get-address64) (address64))
        ((get-zigbee-instruction) (get-zigbee-instruction))
        ))
    
    dispatch))

(define (new-zigbee-message type . args)
  (define (zigbee-vector-to-zigbee-message zigbee-vector)
    (let* ((type (vector-ref zigbee-vector 0))
           (argus '()))
      (cond
        ((equal? type zigbee-transmit-request)
         (set! argus (list type
                           (vector-ref zigbee-vector 1)
                           (subvector zigbee-vector 2 10)
                           (subvector zigbee-vector 10 12)
                           (vector-ref zigbee-vector 12)
                           (vector-ref zigbee-vector 13)
                           (subvector zigbee-vector 14 (vector-length zigbee-vector)))))
        ((equal? type zigbee-transmit-status)
         (set! argus (list type
                           (vector-ref zigbee-vector 1)
                           (subvector zigbee-vector 2 4)
                           (vector-ref zigbee-vector 4)
                           (vector-ref zigbee-vector 5)
                           (vector-ref zigbee-vector 6))))
        ((equal? type zigbee-recieve-paquet)
         (set! argus (list type
                           (subvector zigbee-vector 1 9)
                           (subvector zigbee-vector 9 11)
                           (vector-ref zigbee-vector 11)
                           (subvector zigbee-vector 12 (- (vector-length zigbee-vector) 1))))) ;-1 because last char is line-feed
        (else #f))
      (if (eq? argus '())
          #f
          (apply new-zigbee-message argus))))
  (cond
    ((equal? args '())
     (zigbee-vector-to-zigbee-message type))
    ((equal? type zigbee-transmit-request) (new-zigbee-message-x10 (car args)
                                                                   (car (cdr args))
                                                                   (car (cddr args))
                                                                   (car (cdddr args))
                                                                   (car (cddddr args))
                                                                   (car (cdr (cddddr args)))))
    ((equal? type zigbee-transmit-status) (new-zigbee-message-x8b (car args)
                                                                  (car (cdr args))
                                                                  (car (cddr args))
                                                                  (car (cdddr args))
                                                                  (car (cddddr args))))
    ((equal? type zigbee-recieve-paquet) (new-zigbee-message-x90 (car args)
                                                                 (car (cdr args))
                                                                 (car (cddr args))
                                                                 (car (cdddr args))))
    (else #f)
    ))

