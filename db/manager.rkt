#lang r5rs
(#%require db/base)
(#%require db/sqlite3)
(#%require (rename racket/base rkt:for-each for-each))

(#%require "../internal/steward.rkt")
(#%require "../internal/device.rkt")
(#%require "../internal/sensor.rkt")
(#%require "../internal/actuator.rkt")
(#%require "../internal/element.rkt")
(#%require "../internal/element-type.rkt")
(#%require "../internal/central-unit.rkt")

(#%provide new-db-manager)

(define (initialize)
  (define (connect-to-db)
    (sqlite3-connect #:database "domotica.db"
                     #:mode 'create))
  (define (create-tables)
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "CREATE TABLE Stewards (room char(20), "
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

(define (new-db-manager db-path)
  (define (connect-to-db)
    (sqlite3-connect #:database db-path
                     #:mode 'read/write))
  (define (add-device-type device)
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
  
  (define (add-device device steward)
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
  
  (define (add-steward steward)
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "INSERT INTO Stewards VALUES ('"
                                            (steward 'get-room)
                                            "')"))
      (for-each (lambda (device) (add-device device steward)) (steward 'get-devices))))
  
  (define (add-time-value steward element time value)
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "INSERT INTO ElementTypeValues VALUES ('"
                                            (steward 'get-room) "', "
                                            (number->string element) ", "
                                            (number->string time) ", "
                                            (number->string value) ")"))))
  
  (define (get-steward-time-value steward)
    (let* ((connection (connect-to-db))
           (lst '()))
      (define (res-for-element element)
        (let ((res (query-list connection (string-append "SELECT value FROM ElementTypeValues WHERE room ='"
                                                         (steward 'get-room) "' AND element = "
                                                         (number->string element) " "
                                                         "ORDER BY time ASC")))
              (size 0)
              (vector #f))
          (rkt:for-each (lambda (e) (set! size (+ size 1))) res)
          (set! vector (make-vector size))
          (set! size 0)
          (rkt:for-each (lambda (e) (vector-set! vector size e) (set! size (+ size 1))) res)
          (set! lst (cons vector lst))
          lst))
      (for-each-element-type res-for-element)
      lst))
  
  (define (restore-state)
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
        (rkt:for-each (lambda (s) (set! stewards (cons (new-steward (vector-ref s 0)) stewards))) res))
      (let ((res (query-rows connection "SELECT * FROM Devices")))
        (rkt:for-each (lambda(d) ((find-stewards (vector-ref d 2)) 'add-device (new-device (vector-ref d 1) (vector-ref d 0)))) res))
      (for-each (lambda (s) (central-unit 'add-steward s)) stewards)
      central-unit))
  
  (define (dispatch message . args)
    (case message
      ((add-device-type) (apply add-device-type args))
      ((add-device) (apply add-device args))
      ((add-steward) (apply add-steward args))
      ((add-time-value) (apply add-time-value args))
      ((get-steward-time-value) (apply get-steward-time-value args))
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