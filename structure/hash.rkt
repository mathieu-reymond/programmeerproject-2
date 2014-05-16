#lang r5rs

(#%provide djb2-port
           djb2)

(define port-limit 4000)

(define (djb2 string)
  (define (loop current hash)
    (if (eq? (string-length string) current)
        hash
        (loop (+ current 1)
              (+ (* hash 33) (char->integer (string-ref string current))))))
  (loop 0 5381))

(define (djb2-port string)
  (modulo (djb2 string) port-limit))
