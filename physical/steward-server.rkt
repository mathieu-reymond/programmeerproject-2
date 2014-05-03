#lang r5rs
(#%require racket/tcp
           (only racket/base let-values))
(#%require "../structure/hash.rkt")

(#%provide new-steward-server)

(define (new-steward-server room)
  (let* ((port (djb2-port room))
         (listen (tcp-listen port)))
    (let-values (((in out) (tcp-accept listen)))
      ;temp
      (display (read in))
      (write room out)
      (flush-output out))))