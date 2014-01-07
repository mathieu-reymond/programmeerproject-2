#lang r5rs
(#%require (only racket/base let-values))
(#%require (only racket/base make-pipe))
(#%require (only racket/base make-input-port))

(#%require "../communication/parser.rkt")
(#%require "../internal/instruction.rkt")
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
    ;@param device-serial-number : the serial-number of the device
    ;@return (cons input-port output-port) : a cons with this device's input- and output-ports
    (define (get-ports-for-hardware-device device-serial-number)
      (let loop ((curr map))
        (cond
          ((equal? '() curr) #f) ;no hardware-device with this serial-number
          ((equal? device-serial-number ((vector-ref (car curr) 0) 'get-serial-number)) 
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
;@param serial-number : the device's serial-number, should be unique !
;@param room : the room in which this device is situated
(define (new-hardware-device serial-number room)
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
          (instruction-to-list (new-instruction-ret (inst 'execute room)))))
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
  
  ;get this device's serial-number
  (define (get-serial-number) serial-number)
  
  (define (dispatch message . args)
    (case message
      ((get-serial-number) (get-serial-number))))
  
  ;when creating new hardware, automatically add to device/port map
  (add-to-map)
  dispatch)
