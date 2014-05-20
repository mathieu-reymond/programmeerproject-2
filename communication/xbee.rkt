#lang racket

(provide
 ; Clean Scheme API
 xbee-initialise
 xbee-tick
 xbee-ready?
 xbee-read-frame
 xbee-discover-nodes
 xbee-list-nodes
 xbee-write
 xbee-write-16)

; We need the FFI libraries.
(require ffi/unsafe
         ffi/unsafe/define)

; Set up xbeecomm-define to be a link to our xbee communication library
(define pathtolib (build-path (current-directory) "libxbeeschemelib"))
(define-ffi-definer xbeecomm-define (ffi-lib pathtolib))

; An interface to:
;   int write_api_message_blocking(xbee_dev_t *xbee, const unsigned char target[static 8], const void *msg, int msg_len)
(xbeecomm-define write_api_message_blocking (_fun _pointer _bytes _bytes _int -> _int))
;   int write_api_message16_blocking(xbee_dev_t *xbee, uint16_t target, const char *msg)
(xbeecomm-define write_api_message16_blocking (_fun _pointer _uint16 _bytes -> _int))
;   void *xbee_initialise(const char *serialport, int baudrate)
(xbeecomm-define xbee_initialise (_fun _bytes _int -> _pointer))
;   int xbee_dev_tick( xbee_dev_t *xbee);
(xbeecomm-define xbee_dev_tick (_fun _pointer -> _int))
;   int xbee_disc_discover_nodes( xbee_dev_t *xbee, const char *identifier);
(xbeecomm-define xbee_disc_discover_nodes (_fun _pointer _byte -> _int))
;   int has_payload_scheme(xbee_dev_t *xbee)
(xbeecomm-define has_payload_scheme (_fun _pointer -> _bool))
; void read_vector_payload_scheme(xbee_dev_t *xbee, char **retVector, int *retLength)
(xbeecomm-define read_vector_payload_scheme (_fun _pointer (payload : (_ptr o _bytes))
                                                  -> (len : _int)
                                                  -> (cblock->vector payload _ubyte len)))

(xbeecomm-define xbee_disc_discovered_nodes (_fun (addrs : (_ptr o _pointer))
                                                  (ids :   (_ptr o _pointer))
                                                  -> (num : _int)
                                                  -> (begin0
                                                       (map list
                                                            (cblock->list ids _string num)
                                                            (map (lambda (addr64)
                                                                   (cblock->vector addr64 _byte 8))
                                                                 (cblock->list addrs _pointer num)))
                                                       (free addrs)
                                                       (free ids))))

(define (is-bytevector? obj)
  (and (vector? obj)
       (andmap byte? (vector->list obj))))
         

(define (xbee-list-nodes)
  "Returns an association list of ( <NID string> <addr64 bytevector> ) elements"
  (xbee_disc_discovered_nodes))

(define (xbee-read-frame xbee)
  "Returns a bytevector that contains a frame message"
  (read_vector_payload_scheme xbee))

(define (xbee-ready? xbee)
  "Returns true if data is on the buffer, else false"
  (has_payload_scheme xbee))

(define (xbee-discover-nodes xbee)
  (xbee_disc_discover_nodes xbee 0))

(define (xbee-tick xbee)
  (xbee_dev_tick xbee))

(define (xbee-initialise port rate)
  (let ((port-string (cond ((bytes? port) port)
                           ((string? port) (string->bytes/locale port))
                           (else (error "Expected string or byte string (bytes) as first argument")))))
    (let ((xbee-dev (xbee_initialise port-string rate)))
      (if xbee-dev
          xbee-dev
          (error "Could not initialise xbee device, did you pass the correct device string?")))))

(define (test-args-for-write target message)
  (when (not (is-bytevector? target))
    (error "Pass target address as vector of bytes: e.g. #(0 128 255 1 3 8 7 10)"))
  (when (not (= (vector-length target) 8))
    (error "Target address too small, expecting 8 bytes, gotten:" (vector-length target)))
  (when (not (is-bytevector? message))
    (error "Pass message as vector of bytes: e.g. #(0 128 255 1 3 8 7 10)")))

(define (xbee-write xbee target message)
  (test-args-for-write target message)
  (let ((target (list->bytes (vector->list target)))
        (msg-length (vector-length message))
        (message (list->bytes (vector->list message))))
    (write_api_message_blocking xbee target message msg-length)))

(define (xbee-write-16 xbee target message) 
  (test-args-for-write target message)
  (let ((target (list->bytes (vector->list target)))
        (message (list->bytes (vector->list message))))
    (write_api_message16_blocking xbee target message)))
