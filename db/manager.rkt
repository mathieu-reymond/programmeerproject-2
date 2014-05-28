#lang r5rs
(#%require db/base)
(#%require db/sqlite3)
(#%require (rename racket/base rkt:for-each for-each))
(#%require (only racket/base seconds->date))
(#%require (only racket/date date->seconds))

(#%require "../internal/steward.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/central-unit.rkt")
(#%require "../structure/map.rkt")
(#%require "../rule/rule.rkt")
(#%require "../rule/time-interval.rkt")
(#%require "../rule/recurrence.rkt")

(#%provide new-db-manager)

;b===c* db/db-manager
; NAME
;  db-manager
; DESCRIPTION
;  Een manager die toelaat om gemakkelijk te interageren met de gegevensbank.
;  De manager slaagt de verschillende devices, stewards en resultaten van instructions op.
;e===
;b===mi db/initialize
; NAME
;  initialize
; DESCRIPTION
;  Maakt een nieuwe database aan met de nodige tables.
; SYNOPSIS
(define (initialize)
; SOURCE
  (define (connect-to-db)
    (sqlite3-connect #:database "domotica.db"
                     #:mode 'create))
  (define (create-tables)
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "CREATE TABLE Stewards (room char(20), "
                                            "ip char(20) NOT NULL DEFAULT 'localhost', "
                                            "PRIMARY KEY (room))"))
      (query-exec connection (string-append "CREATE TABLE Devices (serial char(20), "
                                            "name char(20), "
                                            "steward char(20), "
                                            "PRIMARY KEY (serial), "
                                            "FOREIGN KEY (steward) REFERENCES Stewards(room))"))
      (query-exec connection (string-append "CREATE TABLE ElementTypes (type int, "
                                            "sensor bool NOT NULL DEFAULT 0, "
                                            "actuator bool NOT NULL DEFAULT 0, "
                                            "PRIMARY KEY (type))"))
      (query-exec connection (string-append "CREATE TABLE DeviceSensors (device char(20), "
                                            "element int, "
                                            "PRIMARY KEY (device, element), "
                                            "FOREIGN KEY (element) REFERENCES ElementTypes(type))"))
      (query-exec connection (string-append "CREATE TABLE DeviceActuators (device char(20), "
                                            "element int, "
                                            "PRIMARY KEY (device, element), "
                                            "FOREIGN KEY (element) REFERENCES ElementTypes(type))"))
      (query-exec connection (string-append "CREATE TABLE ElementTypeValues (room char(20), "
                                            "element int, "
                                            "time int, "
                                            "value int, "
                                            "FOREIGN KEY (room) REFERENCES Stewards(room))"))
      (query-exec connection (string-append "CREATE TABLE Rules (room char(20), "
                                            "time int, "
                                            "element int, "
                                            "value int, "
                                            "recurrence char(20), "
                                            "end int, "
                                            "PRIMARY KEY (room, time, element), "
                                            "FOREIGN KEY (room) REFERENCES Stewards(room), "
                                            "FOREIGN KEY (element) REFERENCES ElementTypes(type))"))
      
      connection))
  
  (define (fill-ElementTypes)
    (let ((connection (connect-to-db)))
      (define (insert-element-type e-t)
        (query-exec connection (string-append "INSERT INTO ElementTypes VALUES ("
                                              (number->string e-t) ", "
                                              "1, "
                                              "1)")))
      (for-each-element-type insert-element-type)))
  
  (create-tables)
  (fill-ElementTypes))
;e===

;b===o* db-manager/new-db-manager
; NAME
;  new-db-manager
; DESCRIPTION
;  maakt een nieuwe db-manager aan gebaseerd op
;  de database die op db-path ligt.
; PARAMETERS
;  * db-path - het pad van de gegevensbank.
; SYNOPSIS
(define (new-db-manager db-path)
;e===
  (define (connect-to-db)
    (sqlite3-connect #:database db-path
                     #:mode 'read/write))
  ;b===m* db-manager/add-device-type
  ; NAME
  ;  add-device-type
  ; DESCRIPTION
  ;  Voeg een nieuw device-type toe aan de database.
  ; PARAMETERS
  ;  * device - een device van het type die toegevoegd moet worden.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (add-device-type device)
  ; SOURCE
    (let ((connection (connect-to-db)))
      (define (insert-element element)
        (cond
          ((equal? (element 'class) Sensor)
           (if (query-maybe-value connection (string-append "SELECT device FROM DeviceSensors WHERE device = '"
                                                            (device 'get-name) "' AND element = "
                                                            (number->string ((element 'super) 'get-type))))
               'no-need-to-insert
               (query-exec connection (string-append "INSERT INTO DeviceSensors VALUES ('"
                                                     (device 'get-name)
                                                     "', "
                                                     (number->string ((element 'super) 'get-type))
                                                     ")"))))
          ((equal? (element 'class) Actuator)
           (if (query-maybe-value connection (string-append "SELECT device FROM DeviceActuators WHERE device = '"
                                                            (device 'get-name) "' AND element = "
                                                            (number->string ((element 'super) 'get-type))))
               'no-need-to-insert
               (query-exec connection (string-append "INSERT INTO DeviceActuators VALUES ('"
                                                     (device 'get-name)
                                                     "', "
                                                     (number->string ((element 'super) 'get-type))
                                                     ")"))))))
      (for-each insert-element (device 'get-elements))))
  ;e===
  ;b===m* db-manager/add-device
  ; NAME
  ;  add-device
  ; DESCRIPTION
  ;  Voeg een device toe aan de gegevensbank.
  ; PARAMETERS
  ;  * device - de device die toegevoegd moet worden.
  ;  * steward - de steward die het device beheerst.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (add-device device steward)
  ; SOURCE
    (add-device-type device)
    (let ((connection (connect-to-db)))
      (if (query-maybe-value connection (string-append "SELECT serial FROM Devices WHERE serial = '"
                                                       (device 'get-serial-number) "'"))
          (query-exec connection (string-append "UPDATE Devices SET steward = '"
                                                (steward 'get-room) "' WHERE serial = '"
                                                (device 'get-serial-number) "'"))
          (query-exec connection (string-append "INSERT INTO Devices VALUES ('"
                                                (device 'get-serial-number) "', '"
                                                (device 'get-name) "', '"
                                                (steward 'get-room) "')")))))
  ;e===
  ;b===m* db-manager/remove-device
  ; NAME
  ;  remove-device
  ; DESCRIPTION
  ;  Verwijdert een device van de gegevensbank.
  ; PARAMETERS
  ;  * device - de device die verwijdert moet worden.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (remove-device device)
  ; SOURCE
    (let ((connection (connect-to-db)))
      ;if test necessary ?
      (if (query-maybe-value connection (string-append "SELECT serial FROM Devices WHERE serial = '"
                                                       (device 'get-serial-number) "'"))
          (query-exec connection (string-append "DELETE FROM Devices WHERE serial = '"
                                                (device 'get-serial-number) "'"))
          #f)))
  ;e===
  ;b===m* db-manager/get-rules
  ; NAME
  ;  get-rules
  ; DESCRIPTION
  ;  Geeft een list terug met alle rules die op een steward toegepast zijn.
  ; PARAMETERS
  ;  * steward - de steward van wie we de rules willen krijgen
  ; RETURN VALUE
  ;  * list - een lijst met de rules van de steward
  ; SYNOPSIS
  (define (get-rules steward)
  ; SOURCE
    (let* ((connection (connect-to-db))
           (res '()))
      (rkt:for-each (lambda(e) (set! res (cons (new-rule (vector-ref e 2)
                                                         (vector-ref e 3)
                                                         (new-time-interval (seconds->date (vector-ref e 1))
                                                                            (new-recurrence (vector-ref e 4)
                                                                                            (seconds->date (vector-ref e 5)))))
                                               res)))
                    (query-rows connection (string-append "SELECT * FROM Rules WHERE room ='"
                                                          (steward 'get-room) "'")))
      res))
  ;e===
  (define (delete-rules steward)
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "DELETE FROM Rules WHERE room = '"
                                            (steward 'get-room) "'"))))
  ;b===m* db-manager/update-rules
  ; NAME
  ;  update-rules
  ; DESCRIPTION
  ;  Update de rules van een steward in de database.
  ; PARAMETERS
  ;  * steward - de steward van wie we de rules willen updaten.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (update-rules steward)
  ; SOURCE
    (let ((connection (connect-to-db))
          (rule-manager (steward 'get-rule-manager)))
      (delete-rules steward)
      (for-each (lambda(rule)
                  (query-exec connection (string-append "INSERT INTO Rules VALUES ('"
                                                        (steward 'get-room) "', "
                                                        (number->string (date->seconds ((rule 'get-interval) 'get-date))) ", "
                                                        (number->string (rule 'get-element-type)) ", "
                                                        (number->string (rule 'get-value)) ", '"
                                                        (((rule 'get-interval) 'get-recurrence) 'get-type) "', "
                                                        (number->string (date->seconds (((rule 'get-interval) 'get-recurrence) 'get-end))) ")")))
                (rule-manager 'get-rules))))
  ;e===
  ;b===m* db-manager/add-steward
  ; NAME
  ;  add-steward
  ; DESCRIPTION
  ;  Voeg een steward toe aan de gegevensbank.
  ; PARAMETERS
  ;  * steward - de steward die toegevoegd moet worden.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (add-steward steward)
  ; SOURCE
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "INSERT INTO Stewards VALUES ('"
                                            (steward 'get-room) "', '"
                                            (steward 'get-ip)
                                            "')"))
      (for-each (lambda (device) (add-device device steward)) (steward 'get-devices))))
  ;e===
  ;b=== db-manager/remove-steward
  ; NAME
  ;  remove-steward
  ; DESCRIPTION
  ;  Verwijder een steward van de gegevensbank.
  ; PARAMETERS
  ;  * steward - de steward die verwijdert moet worden.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (remove-steward steward)
  ; SOURCE
    (let ((connection (connect-to-db)))
      (delete-rules steward)
      (query-exec connection (string-append "DELETE FROM Stewards WHERE room = '"
                                            (steward 'get-room) "'"))
      (for-each (lambda (device) (remove-device device)) (steward 'get-devices))))
  ;e===
  ;b===m* db-manager/add-time-value
  ; NAME
  ;  add-time-value
  ; DESCRIPTION
  ;  Slaagt de waarde van een bepaalde element-type in een bepaalde kamer op een bepaald tijdstip op
  ;  in de gegevensbank.
  ; PARAMETERS
  ;  * steward - de steward die het element-type gemeten heeft.
  ;  * element - het element-type die gemeten werd.
  ;  * time - het tijdstip (in seconden) van de meting.
  ;  * value - de waarde van de meting.
  ; RETURN VALUE
  ;  #<void>
  ; SYNOPSIS
  (define (add-time-value steward element time value)
  ; SOURCE
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "INSERT INTO ElementTypeValues VALUES ('"
                                            (steward 'get-room) "', "
                                            (number->string element) ", "
                                            (number->string time) ", "
                                            (number->string value) ")"))))
  ;e===
  ;b===* db-manager/get-steward-time-value
  ; NAME
  ;  get-steward-time-value
  ; DESCRIPTION
  ;  Geeft een map terug met de metingen van de element-types in een kamer.
  ;
  ;  De map opstelling : (element-type >< (list (vector time value) ...))
  ; PARAMETERS
  ;  steward - de steward van de gevraagde kamer.
  ; RETURN VALUE
  ;  map - een map met de metingen
  ; SYNOPSIS
  (define (get-steward-time-value steward)
  ; SOURCE
    (let* ((connection (connect-to-db))
           (vals (new-map)))
      (define (res-for-element element)
        (let ((res (query-rows connection (string-append "SELECT time, value FROM ElementTypeValues WHERE room ='"
                                                         (steward 'get-room) "' AND element = "
                                                         (number->string element) " "
                                                         "ORDER BY time ASC"))))
          (vals 'add! element res)))
      (for-each-element-type res-for-element)
      vals))
  ;e===
  
  ;b===m* db-manager/restore-state
  ; NAME
  ;  restore-state
  ; DESCRIPTION
  ;  Geeft een central-unit terug met de huidige stand van de gegevensbank.
  ;  De central-unit beheerst dus alle verschillende opgeslagen stewards en opgeslagen devices.
  ; RETURN VALUE
  ;  central-unit - een central-unit met de huidige stand van de database.
  ; SYNOPSIS
  (define (restore-state)
  ; SOURCE
    (let ((connection (connect-to-db))
          (stewards '())
          (devices '())
          (central-unit (new-central-unit)))
      (define (find-stewards name)
        (let ((found #f))
          (for-each (lambda (s) (if (equal? (s 'get-room) name) (set! found s) #f)) stewards)
          found))
      (define (find-devices name)
        (let ((found #f))
          (for-each (lambda (d) (if (equal? (d 'get-name) name) (set! found d) #f)) devices)
          found))
      
      (let ((res (query-rows connection "SELECT * FROM DeviceSensors")))
        (define (update-device ds)
          (if (not (find-devices (vector-ref ds 0))) ;no device with this name yet
              (set! devices (cons (new-device (vector-ref ds 0) (vector-ref ds 0)) devices))
              'already-in-devices)
          ((find-devices (vector-ref ds 0)) 'add-element (new-sensor (vector-ref ds 1))))
        (rkt:for-each update-device res))
      (let ((res (query-rows connection "SELECT * FROM DeviceActuators")))
        (define (update-device ds)
          (if (not (find-devices (vector-ref ds 0)))
              (set! devices (cons (new-device (vector-ref ds 0) (vector-ref ds 0)) devices))
              'already-in-devices)
          ((find-devices (vector-ref ds 0)) 'add-element (new-actuator (vector-ref ds 1))))
        (rkt:for-each update-device res))
      (let ((res (query-rows connection "SELECT * FROM Stewards")))
        (rkt:for-each (lambda (s) (set! stewards (cons (new-steward (vector-ref s 0) (vector-ref s 1)) stewards))) res))
      (let ((res (query-rows connection "SELECT * FROM Devices")))
        (rkt:for-each (lambda(d) ((find-stewards (vector-ref d 2)) 'add-device (new-device (vector-ref d 1) (vector-ref d 0)))) res))
      (for-each (lambda (s)
                  (let ((rules (get-rules s))
                        (rule-manager (s 'get-rule-manager)))
                    (for-each (lambda(r) (rule-manager 'add-rule r)) rules))) stewards)
      (for-each (lambda (s) (central-unit 'add-steward s)) stewards)
      central-unit))
  ;e===
  
  (define (dispatch message . args)
    (case message
      ((add-device-type) (apply add-device-type args))
      ((add-device) (apply add-device args))
      ((remove-device) (apply remove-device args))
      ((add-steward) (apply add-steward args))
      ((remove-steward) (apply remove-steward args))
      ((add-time-value) (apply add-time-value args))
      ((get-steward-time-value) (apply get-steward-time-value args))
      ((get-rules) (apply get-rules args))
      ((update-rules) (apply update-rules args))
      ((restore-state) (restore-state))))
  
  dispatch)

;;INITIALIZE WITH DATA
;
;(initialize)
;
;(define thermostat (new-device "thermostat" "thermostat"))
;(thermostat 'add-element (new-sensor 0))
;(thermostat 'add-element (new-actuator 0))
;(define light-detector (new-device "light-detector" "light-detector"))
;(light-detector 'add-element (new-sensor 1))
;(define bedroom (new-steward "bedroom"))
;(bedroom 'add-device (new-device "light-detector" "123456"))
;(define kitchen (new-steward "kitchen"))
;(kitchen 'add-device (new-device "thermostat" "654321"))
;(define living-room (new-steward "living-room"))
;(living-room 'add-device (new-device "thermostat" "456789"))
;(living-room 'add-device (new-device "light-detector" "987654"))
;
;(define m (new-db-manager "domotica.db"))
;(m 'add-steward bedroom)
;(m 'add-steward kitchen)
;(m 'add-steward living-room)
;
;(display "FROM DATABASE :") (newline)
;(((new-db-manager "domotica.db") 'restore-state) 'for-each-steward (lambda(s) (display (s 'get-room)) 
;                                                       (display " :") 
;                                                       (newline) 
;                                                       (for-each (lambda(d) 
;                                                                   (display (d 'get-serial-number)) 
;                                                                   (display " : ") 
;                                                                   (display (length (d 'get-elements))) 
;                                                                   (newline)) (s 'get-devices))))