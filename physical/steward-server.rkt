#lang r5rs
(#%require racket/tcp
           (only racket/base let-values))
(#%require "../structure/hash.rkt"
           "../communication/parser.rkt"
           "../internal/instruction.rkt"
           "physical-room.rkt")

(#%provide new-steward-server)

(define (new-steward-server room)
  (let* ((port (djb2-port room))
         (listen (tcp-listen port))
         (p-room (new-physical-room room)))
    (let-values (((in out) (tcp-accept listen)))
      (define (process-request)
        (let* ((request (read in))
               (inst (list-to-instruction (cdr request))))
          ;returns answer
          (instruction-to-list (new-instruction-ret (inst 'execute p-room)))))
      (define (loop)
        (let ((ret (process-request)))
          (display ret)
          (write ret out)
          (flush-output out))
        (loop))
      (loop)
      )))