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

;b===v* zigbee-message/zigbee-transmit-request
; NAME
;  zigbee-transmit-request
; DESCRIPTION
;  Het type van het zigbee-transmit-request bericht.
;  (zie zigbee-message-x10)
; SOURCE
(define zigbee-transmit-request 16) ;0x10
;e===
;b===v* zigbee-message/zigbee-transmit-status
; NAME
;  zigbee-transmit-status
; DESCRIPTION
;  Het type van het zigbee-transmit-status bericht.
;  (zie zigbee-message-x8b)
; SOURCE
(define zigbee-transmit-status 139) ;0x8b
;e===
;b===v* zigbee-message/zigbee-recieve-paquet
; NAME
;  zigbee-recieve-paquet
; DESCRIPTION
;  Het type van het zigbee-recieve-paquet bericht.
;  (zie zigbee-message-x90)
; SOURCE
(define zigbee-recieve-paquet 144) ;0x90
;e===
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

;b===im* zigbee-message/split-zigbee-string
; NAME
;  split-zigbee-string
; DESCRIPTION
;  Split een zigbee string in verschillende delen :
;   * acknowledged
;   * set/get
;   * element-type (POW, WORK...)
;   * waarde van de element-type (ON, OFF...)
;  Er wordt een vector teruggegeven van lengte 4 met op positie :
;   * 0 : #f, "ACK" of "NACK"
;   * 1 : "SET" of "GET"
;   * 2 : list ("POW", "WORK",...)
;   * 3 : map ("POW",... >< "ON", "OFF",...)
; PARAMETERS
;  * zigbee-string - de string die gesplitst moet worden
; RETURN VALUE
;  vector - een vector met de gesplitste data
; EXAMPLE
;  "ACK: SET POW=ON\n" geeft een vector met :
;   * "ACK"
;   * "SET"
;   * (list "POW")
;   * ((new-map) 'add! "POW" (list 1))
; SYNOPSIS
(define (split-zigbee-string zigbee-string)
; SOURCE
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
           parsed)))
      (loop 0 0 0))))
;e===

;b===m* zigbee-message/zigbee-vector-to-zigbee-string
; NAME
;  zigbee-vector-to-zigbee-string
; DESCRIPTION
;  zet een zigbee bytevector om naar zijn string-representatie
; PARAMETERS
;  * zigbee-vector - de bytevector die omgezet moet worden
; RETURN VALUE
;  string - een zigbee-string
; SYNOPSIS
(define (zigbee-vector-to-zigbee-string zigbee-vector)
; SOURCE
  (let ((vec (make-vector (vector-length zigbee-vector))))
    (define (to-char current)
      (cond
        ((eq? current (vector-length vec)) #t)
        (else (vector-set! vec current (integer->char (vector-ref zigbee-vector current)))
              (to-char (+ current 1)))))
    (to-char 0)
    (apply string (vector->list vec))))
;e===
;b===c* communication/zigbee-instruction
; NAME
;  zigbee-instruction
; DESCRIPTION
;  Een class die een zigbee instructie voorstelt.
;  De zigbee-string wordt naar een object omgezet zodanig dat de verschillende
;  onderdelen van de string gemakkelijk opvraagbaar worden.
;e===
;b===o* zigbee-instruction/new-zigbee-instruction
; NAME
;  new-zigbee-instruction
; DESCRIPTION
;  Maakt een nieuw zigbee-instruction object aan.
; PARAMETERS
;  * zigbee-string - de zigbee-string die door deze instructie voorgesteld gaat worden.
; SYNOPSIS
(define (new-zigbee-instruction zigbee-string)
;e===
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
    ;b===m* zigbee-instruction/to-string
    ; NAME
    ;  to-string
    ; DESCRIPTION
    ;  geeft de een string terug die deze instructie voorstelt.
    ; RETURN VALUE
    ;  string - de zigbee string
    ; SYNOPSIS
    (define (to-string)
    ; SOURCE
      zigbee-string)
    ;e===
    ;b===m* zigbee-instruction/to-vector
    ; NAME
    ;  to-vector
    ; DESCRIPTION
    ;  geeft de bytevector van de string voorstelling van deze instructie terug.
    ; RETURN VALUE
    ;  vector - de bytevector
    ; SYNOPSIS
    (define (to-vector)
    ; SOURCE
      vect)
    ;e===
    ;b===m* zigbee-instruction/acknowledged?
    ; NAME
    ;  acknowledged?
    ; DESCRIPTION
    ;  Is deze instructie een ACK instructie ?
    ;  bv. "ACK: SET POW=ON\n" geeft #t terug, maar "GET\n" niet.
    ; RETURN VALUE
    ;  vector - de bytevector
    ; SYNOPSIS
    (define (acknowledged?)
    ; SOURCE
      (equal? "ack" (if (string? (vector-ref split 0))
                                              (string-downcase (vector-ref split 0))
                                              #f)))
    ;e===
    ;b===m* zigbee-instruction/type
    ; NAME
    ;  type
    ; DESCRIPTION
    ;  Geeft het type terug van de zigbee-string
    ;  bv. "ACK: SET POW=ON\n" geeft 'set terug, "GET\n" geeft 'get.
    ; RETURN VALUE
    ;  symbol - de symbol representatie van het type.
    ; SYNOPSIS
    (define (type)
    ; SOURCE
      (string->symbol (string-downcase (vector-ref split 1))))
    ;e===
    (define (element-types) (map (lambda(e) (element-type-zigbee-type-map 'key e)) (vector-ref split 2)))
    ;b===m* zigbee-instruction/values
    ; NAME
    ;  values
    ; DESCRIPTION
    ;  Geeft het een lijst met de waarden van de zigbee-string voor een bepaald element-type terug.
    ;  bv. "ACK: SET POW=ON\n" geeft (list 1) terug voor TEMPERATURE, "GET\n" geeft zowiezo '().
    ; PARAMETERS
    ;  * element-type - het gevraagde element-type.
    ; RETURN VALUE
    ;  list - lijst met waarden.
    ; SYNOPSIS
    (define (values element-type)
    ; SOURCE
      ((vector-ref split 3) 'find (element-type-zigbee-type-map 'find element-type)))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((to-string) (to-string))
        ((to-vector) (to-vector))
        ((acknowledged?) (acknowledged?))
        ((type) (type))
        ((element-types) (element-types))
        ((values) (apply values args))))
    
    dispatch))

;b===m* zigbee-instruction/execute-zigbee-instruction
; NAME
;  execute-zigbee-instruction
; DESCRIPTION
;  Voer een zigbee-instruction uit.
;  Dit wordt alleen gebruikt tijdens het simuleren van de hardware,
;  omdat het xbee toestel normaalgezien de zigbee instructies vanzelf uitvoert.
; PARAMETERS
;  * zigbee-instruction - de zigbee-instruction die uitgevoerd moet worden.
;  * phys-room - de physical-room waarin de instructie uitgevoerd moet worden.
; RETURN VALUE
;  zigbee-string - het gesimuleerde antwoord van de hardware : een string die het zigbee protocol volgt.
; SYNOPSIS
(define (execute-zigbee-instruction zigbee-instruction phys-room)
; SOURCE
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
;e===

;b===c* communication/zigbee-message-x10
; NAME
;  zigbee-message-x10
; DESCRIPTION
;  Een classe die een zigbee bytevector van type x10 voorstelt.
;  De opstelling van een zigbee vector (x10) :
;   * 0 : het type (in dit geval x10/16)
;   * 1 : de frame id
;   * 2-9 : 64bit adres
;   * 10-11 : 16bit adres
;   * 12 : broadcast-radius
;   * 13 : opties
;   * 14-* : de bytevector representatie van een zigbee string
; PARENTS
;  * zigbee-message
;e===
;b===o* zigbee-message-x10/new-zigbee-message-x10
; NAME
;  new-zigbee-message-x10
; DESCRIPTION
;  Maakt een nieuw zigbee-message-x10 object aan.
; PARAMETERS
;  * id - de id van het bericht
;  * address64 - het 64bit adres
;  * address16 - het 16bit adres
;  * broadcast-radius - de broadcast-radius
;  * options - de opties
;  * zigbee-vector - de bytevector representatie van een zigbee string
; SYNOPSIS
(define (new-zigbee-message-x10 id address64 address16 broadcast-radius options zigbee-vector)
;e===
  (let ((vect (vector-append (vector zigbee-transmit-request)
                             (vector id)
                             address64
                             address16
                             (vector broadcast-radius)
                             (vector options)
                             zigbee-vector))
        (split (split-zigbee-string (zigbee-vector-to-zigbee-string zigbee-vector))))
    ;b===m* zigbee-message-x10/type
    ; NAME
    ;  type
    ; DESCRIPTION
    ;  Geeft het type terug van de instructie (in dit geval zigbee-transmit-request).
    ; RETURN VALUE
    ;  integer - het type van deze instructie
    ; SYNOPSIS
    (define (type)
    ; SOURCE
      zigbee-transmit-request)
    ;e===
    ;b===m* zigbee-message-x10/to-vector
    ; NAME
    ;  to-vector
    ; DESCRIPTION
    ;  De bytevector representatie van deze zigbee instructie.
    ; RETURN VALUE
    ;  vector - een bytevector die deze instructie voorstelt.
    ; SYNOPSIS
    (define (to-vector)
    ; SOURCE
      vect)
    ;e===
    ;b===m* zigbee-message-x10/get-address64
    ; NAME
    ;  get-address64
    ; DESCRIPTION
    ;  Het 64 bit adres van deze zigbee instructie.
    ; RETURN VALUE
    ;  vector - een bytevector met het 64 bit adres.
    ; SYNOPSIS
    (define (get-address64)
    ; SOURCE
      address64)
    ;e===
    ;b===m* zigbee-message-x10/get-zigbee-instruction
    ; NAME
    ;  get-zigbee-instruction
    ; DESCRIPTION
    ;  Geeft een zigbee-instruction object terug van deze message.
    ; RETURN VALUE
    ;  zigbee-instruction - de instructie van dit bericht.
    ; SYNOPSIS
    (define (get-zigbee-instruction)
    ; SOURCE
      (new-zigbee-instruction (zigbee-vector-to-zigbee-string zigbee-vector)))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((type) (type))
        ((to-vector) (to-vector))
        ((get-address64) (get-address64))
        ((get-zigbee-instruction) (get-zigbee-instruction))
        ))
    
    dispatch))

;b===c* communication/zigbee-message-x8b
; NAME
;  zigbee-message-x8b
; DESCRIPTION
;  Een classe die een zigbee bytevector van type x8b voorstelt.
;  De opstelling van een zigbee vector (x8b) :
;   * 0 : het type (in dit geval x8b/139)
;   * 1 : de frame id
;   * 2-3 : 16bit adres
;   * 4 : retry-count
;   * 5 : delivery-status (x00 is delivered, x24 is address not found)
;   * 6 : discovery-status
; PARENTS
;  * zigbee-message
;e===
;b===o* zigbee-message-x8b/new-zigbee-message-x8b
; NAME
;  new-zigbee-message-x8b
; DESCRIPTION
;  Maakt een nieuw zigbee-message-x8b object aan.
; PARAMETERS
;  * id - de id van het bericht
;  * address16 - het 16bit adres
;  * retry-count - het aantal keren dat het bericht verzonden werd
;  * delivery-status - x00 is delivered, x24 is address not found
;  * discovery-status - het discovery-status
; SYNOPSIS
(define (new-zigbee-message-x8b id address16 retry-count delivery-status discovery-status)
;e===
  (let ((vect (vector-append (vector zigbee-transmit-status)
                             (vector id)
                             address16 
                             (vector retry-count)
                             (vector delivery-status)
                             (vector discovery-status))))
    ;b===m* zigbee-message-x8b/type
    ; NAME
    ;  type
    ; DESCRIPTION
    ;  Geeft het type terug van de instructie (in dit geval zigbee-transmit-status).
    ; RETURN VALUE
    ;  integer - het type van deze instructie
    ; SYNOPSIS
    (define (type)
    ; SOURCE
      zigbee-transmit-status)
    ;e===
    ;b===m* zigbee-message-x8b/to-vector
    ; NAME
    ;  to-vector
    ; DESCRIPTION
    ;  De bytevector representatie van deze zigbee instructie.
    ; RETURN VALUE
    ;  vector - een bytevector die deze instructie voorstelt.
    ; SYNOPSIS
    (define (to-vector) 
    ; SOURCE
      vect)
    ;e===
    ;b===m* zigbee-message-x8b/delivered?
    ; NAME
    ;  delivered?
    ; DESCRIPTION
    ;  Geeft aan of het bericht aangekomen is.
    ; RETURN VALUE
    ;  #t - true wanneer het aangekomen is.
    ;  #f - false wanneer het niet aangekomen is.
    ; SYNOPSIS
    (define (delivered?)
    ; SOURCE
      (eq? delivery-status 0))
    ;e===
    ;b===m* zigbee-message-x8b/get-retry-count
    ; NAME
    ;  get-retry-count
    ; DESCRIPTION
    ;  Geeft het aantal retries van het bericht.
    ; RETURN VALUE
    ;  integer - het retry count.
    ; SYNOPSIS
    (define (get-retry-count) 
    ; SOURCE
      retry-count)
    ;e===
    (define (dispatch message . args)
      (case message
        ((type) (type))
        ((to-vector) (to-vector))
        ((delivered?) (delivered?))
        ((get-retry-count) (get-retry-count))
        ))
    
    dispatch))
;b===c* communication/zigbee-message-x90
; NAME
;  zigbee-message-x90
; DESCRIPTION
;  Een classe die een zigbee bytevector van type x90 voorstelt.
;  De opstelling van een zigbee vector (x90) :
;   * 0 : het type (in dit geval x90/144)
;   * 1-8 : 64bit adres
;   * 9-10 : 16bit adres
;   * 11 : recieve option
;   * 12-* : de bytevector van een zigbee-instruction
; PARENTS
;  * zigbee-message
;e===
;b===o* zigbee-message-x90/new-zigbee-message-x90
; NAME
;  new-zigbee-message-x90
; DESCRIPTION
;  Maakt een nieuw zigbee-message-x90 object aan.
; PARAMETERS
;  * address64 - het 64bit adres
;  * address16 - het 16bit adres
;  * recieve-option - de recieve-option
;  * zigbee-vector - de bytevector van een zigbee-string
; SYNOPSIS
(define (new-zigbee-message-x90 address64 address16 recieve-option zigbee-vector)
;e===
  (let ((vect (vector-append (vector zigbee-recieve-paquet)
                             address64
                             address16
                             (vector recieve-option)
                             zigbee-vector
                             (vector line-feed)))
        (split (split-zigbee-string (zigbee-vector-to-zigbee-string zigbee-vector))))
    ;b===m* zigbee-message-x90/type
    ; NAME
    ;  type
    ; DESCRIPTION
    ;  Geeft het type terug van de instructie (in dit geval zigbee-recieve-paquet).
    ; RETURN VALUE
    ;  integer - het type van deze instructie
    ; SYNOPSIS
    (define (type)
    ; SOURCE
      zigbee-recieve-paquet)
    ;e===
    ;b===m* zigbee-message-x90/to-vector
    ; NAME
    ;  to-vector
    ; DESCRIPTION
    ;  De bytevector representatie van deze zigbee instructie.
    ; RETURN VALUE
    ;  vector - een bytevector die deze instructie voorstelt.
    ; SYNOPSIS
    (define (to-vector)
    ; SOURCE
      vect)
    ;e===
    ;b===m* zigbee-message-x90/get-address64
    ; NAME
    ;  get-address64
    ; DESCRIPTION
    ;  Het 64bit adres van de ontvanger van dit bericht.
    ; RETURN VALUE
    ;  vector - een vector met het 64bit adres van de ontvanger
    ; SYNOPSIS
    (define (get-address64)
    ; SOURCE
      address64)
    ;e===
    ;b===m* zigbee-message-x90/get-zigbee-instruction
    ; NAME
    ;  get-zigbee-instruction
    ; DESCRIPTION
    ;  Geeft een zigbee-instruction object terug van deze message.
    ; RETURN VALUE
    ;  zigbee-instruction - de instructie van dit bericht.
    ; SYNOPSIS
    (define (get-zigbee-instruction)
    ; SOURCE
      (new-zigbee-instruction (zigbee-vector-to-zigbee-string zigbee-vector)))
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((type) (type))
        ((to-vector) (to-vector))
        ((get-address64) (address64))
        ((get-zigbee-instruction) (get-zigbee-instruction))
        ))
    
    dispatch))
;b===c* communication/zigbee-message
; NAME
;  zigbee-message
; DESCRIPTION
;  Een classe die een zigbee bytevector voorstelt.
; CHILDREN
;  * zigbee-message-x10
;  * zigbee-message-x8b
;  * zigbee-message-x90
;e===
;b===o* zigbee-message/new-zigbee-message
; NAME
;  new-zigbee-message
; DESCRIPTION
;  Maakt een nieuw zigbee-message aan in functie van het meegegeven type.
;  Dus voor type :
;   * zigbee-transmit-request wordt new-zigbee-message-x10 opgeroepen
;   * zigbee-transmit-status wordt new-zigbee-message-x8b opgeroepen
;   * zigbee-recieve-paquet wordt new-zigbee-message-x90 opgeroepen
;  Als er maar een vector meegegeven wordt als parameter wordt er
;  veronderstelt dat deze een bytevector van een zigbee-message is.
;  De eerste byte wordt gelezen en als type van de zigbee-message gezien.
;  De vector wordt dan gesplitst en de correcte constructor opgeroepen.
; PARAMETERS
;  * type - het type van de message
;  * args - de argumenten van de verschillende zigbee-messages.
;           In het geval dat er geen extra argumenten zijn wordt type gezien als een zigbee vector.
; SYNOPSIS
(define (new-zigbee-message type . args)
;e===
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

