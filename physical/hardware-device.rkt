#lang r5rs

(#%require "../communication/parser.rkt")
(#%provide new-hardware-device)
(#%provide hardware-device/port-map)

;creates a map
;KEY : hardware-device, VALUES : input-port, output-port
(define (new-hardware-device/port-map)
  (let ((map '()))
    ;add a device to the map
    (define (add device in-port out-port)
      (set! map (cons (vector device in-port out-port) map)))
    
    ;get ports of corresponding device
    ;@param device-name : the name of the device
    ;@return (list input-port output-port) : a list with this device's input- and output-ports
    (define (get-ports-for-hardware-device device-name)
      (let loop ((curr map))
        (cond
          ((equal? '() curr) #f) ;no hardware-device with this name
          ((equal? device-name ((vector-ref (car curr) 0) 'get-name)) 
           (cons (vector-ref (car curr) 1) (vector-ref (car curr) 2))) ;returns (list in-port out-port)
          (else (loop (cdr curr))))))
    
    (define (dispatch message . args)
      (case message
        ((add) (apply add args))
        ((get-ports-for-hardware-device) (apply get-ports-for-hardware-device args))))
    
    dispatch))

;the global map containing all hardware-devices and their ports
;every new hardware-device is automatically added to this map
(define hardware-device/port-map (new-hardware/port-map))

;hardware-device emulates how the real hardware should work :
; it reads requests from it's port,
; process the request in it's environment (in this case, the room),
;  ex: get the current room temperature
; send an acknowledgment using it's ports.
;  ex: send the room temperature
;@param name : the device's name, should be unique !
;@param room : the room in which this device is situated
;@param in-port : the port this device will use to read
;@param out-port : the port this device will use to write
(define (new-hardware-device name room in-port out-port)
  ;get this device's name
  (define (get-name) name)
  ;process a request :
  ; read from it's port, 
  ; analyse and process message,
  ; send answer back on port
  (define (process-request)
    (let* ((request (read in-port))
           (inst (list-to-instruction request)))
      ;recieved instruction, process using room as param (ex: (room 'get-temperature), then send acknowledgment)
      (write (instruction-to-list inst) out-port))) ;temp
  
  (define (dispatch message . args)
    (case message
      ((get-name) (get-name))
      ((process-request) (process-request))))
  
  ;when creating new hardware, automatically add to device/port map
  (hardware-device/port-map 'add dispatch in-port out-port)
  dispatch)
