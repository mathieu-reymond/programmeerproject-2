#lang r5rs
;(require rnrs/mutable-pairs-6)
(#%require (only racket/base error))

(#%provide new-map)

(define (new-map)
  (let ((structure '()))
    (define (add! key element)
      (set! structure (cons (cons key element) structure)))
    (define (remove! key)
      (let loop ((previous '())
                 (current structure))
        (cond
          ((eq? '() current) #f)
          ((equal? key (car (car current)))
           (if (eq? '() previous)
               (set! structure (cdr current))
               (set-cdr! previous (cdr current))))
          (else (loop current (cdr current))))))
    (define (find key)
      (let loop ((current structure))
        (cond
          ((eq? '() current) #f)
          ((equal? key (car (car current)))
           (cdr (car current)))
          (else (loop (cdr current))))))
    (define (get-elements)
      (map (lambda (el) (cdr el)) structure)) 
    (define (get-keys)
      (map (lambda (el) (car el)) structure))
    
    (define (dispatch message . args)
      (case message
        ((add!) (apply add! args))
        ((remove!) (apply remove! args))
        ((find) (apply find args))
        ((get-elements) (get-elements))
        ((get-keys) (get-keys))
        (else (error "Error : Map.class : unknown method : " message))))
    
    dispatch))