#lang r5rs
(#%require (only racket/base let-values))
(#%require (only racket/base make-pipe))
(#%require (only racket/base make-input-port))

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
    ;@return (cons input-port output-port) : a cons with this device's input- and output-ports
    (define (get-ports-for-hardware-device device-name)
      (let loop ((curr map))
        (cond
          ((equal? '() curr) #f) ;no hardware-device with this name
          ((equal? device-name ((vector-ref (car curr) 0) 'get-name)) 
           (cons (vector-ref (car curr) 1) (vector-ref (car curr) 2))) ;returns (cons in-port out-port)
          (else (loop (cdr curr))))))
    
    (define (dispatch message . args)
      (case message
        ((add) (apply add args))
        ((get-ports-for-hardware-device) (apply get-ports-for-hardware-device args))))
    
    dispatch))

;the global map containing all hardware-devices and their ports
;every new hardware-device is automatically added to this map
(define hardware-device/port-map (new-hardware-device/port-map))

;hardware-device emulates how the real hardware should work :
; it reads requests from it's port,
; process the request in it's environment (in this case, the room),
;  ex: get the current room temperature
; send an acknowledgment using it's ports.
;  ex: send the room temperature
;@param name : the device's name, should be unique !
;@param room : the room in which this device is situated
(define (new-hardware-device name room)
  ;when creating a new hardware-device,
  ;open ports to communicate with this device,
  ;add ports to map
  (define (add-to-map)
    ;the pipe to communicate with this hardware-device
    (let-values (((input-port output-port) (make-pipe)))
      (define (process-request)
        (let* ((request (read input-port))
               (inst (list-to-instruction request)))
          ;returns answer
          (get-name))) ;temp
      ;a custom port is needed to process what is written on the output port
      ;and answer accordingly
      (let ((custom-input-port (make-input-port
                                ;name of the port
                                'hardware-port
                                ;reading procedure
                                (lambda (ignore)
                                  ;local pipe
                                  (let-values (((in out) (make-pipe)))
                                    ;process request
                                    (write (process-request) out)
                                    ;returns the local input port
                                    in))
                                ;peek procedure (not needed)
                                #f
                                ;close procedure
                                (lambda () 'closed))))
        ;automatically add to device/port map
        (hardware-device/port-map 'add dispatch custom-input-port output-port))))
  
  ;get this device's name
  (define (get-name) name)
  
  (define (dispatch message . args)
    (case message
      ((get-name) (get-name))))
  
  ;when creating new hardware, automatically add to device/port map
  (add-to-map)
  dispatch)
