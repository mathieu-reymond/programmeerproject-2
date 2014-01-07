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

(define (connect-to-db)
  (sqlite3-connect #:database "domotica.db"
                   #:mode 'create))

(define (initialize)
  
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
                                            "FOREIGN KEY(device) REFERENCES Devices(serial), "
                                            "FOREIGN KEY (element) REFERENCES ElementTypes(type))"))
      (query-exec connection (string-append "CREATE TABLE DeviceActuators (device char(20), "
                                            "element int, "
                                            "PRIMARY KEY (device, element), "
                                            "FOREIGN KEY(device) REFERENCES Devices(serial), "
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

(define (new-db-manager)
  
  (define (add-device device steward)
    (let ((connection (connect-to-db)))
      (define (insert-element element)
        (cond
          ((equal? (element 'class) Sensor)
           (query-exec connection (string-append "INSERT INTO DeviceSensors VALUES ('"
                                                 (device 'get-serial-number)
                                                 "', "
                                                 (number->string ((element 'super) 'get-type))
                                                 ")")))
          ((equal? (element 'class) Actuator)
           (query-exec connection (string-append "INSERT INTO DeviceActuators VALUES ('"
                                                 (device 'get-serial-number)
                                                 "', "
                                                 (number->string ((element 'super) 'get-type))
                                                 ")")))))
      (query-exec connection (string-append "INSERT INTO Devices VALUES ('"
                                            (device 'get-serial-number) "', '"
                                            (device 'get-name) "', '"
                                            (steward 'get-room) "')"))
      (for-each insert-element (device 'get-elements))))
  
  (define (add-steward steward)
    (let ((connection (connect-to-db)))
      (query-exec connection (string-append "INSERT INTO Stewards VALUES ('"
                                            (steward 'get-room)
                                            "')"))
      (for-each (lambda (device) (add-device device steward)) (steward 'get-devices))))
  
  (define (restore-state)
    (let ((connection (connect-to-db))
          (stewards '())
          (devices '())
          (central-unit (new-central-unit)))
      (define (find-stewards name)
        (let ((found #f))
          (for-each (lambda (s) (if (equal? (s 'get-room) name) (set! found s) #f)) stewards)
          found))
      (define (find-devices serial)
        (let ((found #f))
          (for-each (lambda (d) (if (equal? (d 'get-serial-number) serial) (set! found d) #f)) devices)
          found))
      (let ((res (query-rows connection "SELECT * FROM Stewards")))
        (rkt:for-each (lambda (s) (set! stewards (cons (new-steward (vector-ref s 0)) stewards))) res))
      (let ((res (query-rows connection "SELECT * FROM Devices")))
        (rkt:for-each (lambda (d) (begin (set! devices (cons (new-device (vector-ref d 1) (vector-ref d 0)) devices))
                                     ((find-stewards (vector-ref d 2)) 'add-device (find-devices (vector-ref d 0))))) res))
      (let ((res (query-rows connection "SELECT * FROM DeviceSensors")))
        (rkt:for-each (lambda (ds) ((find-devices (vector-ref ds 0) ) 'add-element (new-sensor (vector-ref ds 1)))) res))
      (let ((res (query-rows connection "SELECT * FROM DeviceActuators")))
        (rkt:for-each (lambda (da) ((find-devices (vector-ref da 0) ) 'add-element (new-actuator (vector-ref da 1)))) res))
      (for-each (lambda (s) (central-unit 'add-steward s)) stewards)
      central-unit))
  
  (define (dispatch message . args)
    (case message
      ((add-device) (apply add-device args))
      ((add-steward) (apply add-steward args))
      ((restore-state) (restore-state))))
  
  dispatch)