#lang r5rs

(#%require "../structure/map.rkt"
           "../structure/hash.rkt")

(#%provide new-hardware-device
           hardware-device-map)

(define hardware-device-map (new-map))

(define (new-address64 string)
  (let ((number (djb2 string))
        (vector (make-vector 8)))
    (define (loop current nr nm)
      (cond
        ((eq? current (vector-length vector)) vector)
        (else
         (vector-set! vector current nm)
         (loop (+ current 1) (floor (/ nr 256)) (modulo nr 256)))))
    (loop 0 (floor (/ number 256)) (modulo number 256))))


(define (new-hardware-device serial-number room)
  (let ((address64 (new-address64 serial-number)))
    (define (get-serial-number) serial-number)
    (define (get-address64) address64)
    (define (get-room) room)
    
    (define (dispatch message . args)
      (case message
        ((get-serial-number) (get-serial-number))
        ((get-address64) (get-address64))
        ((get-room) (get-room))))
    
    (hardware-device-map 'add! (get-serial-number) dispatch)
    dispatch))
      