#lang racket
(require racket/gui/base)
(require plot)
(require "../structure/map.rkt")
(require "panel-list.rkt")

(provide new-graph)
(provide panel-graph)

(define (new-graph)
  (let ((maps (new-map)))
    (define (add! key map)
      (maps 'add! key map))
    (define (find key) (maps 'find key))
    (define (get-maps)
      ;needed to convert r5rs to racket
      (let loop ((current (maps 'get-elements))
                 (res '()))
        (if (eq? '() current)
            res
            (loop (mcdr current) (cons (mcar current) res)))))
    (define (get-keys)
      ;needed to convert r5rs to racket
      (let loop ((current (maps 'get-keys))
                 (res '()))
        (if (eq? '() current)
            res
            (loop (mcdr current) (cons (mcar current) res)))))
    
    (define (map-to-line map)
      (let loop ((result '())
                 (current (map 'get-keys)))
        (cond
          ((eq? '() current) result)
          (else
           (loop (cons (vector (mcar current) 
                               (map 'find (mcar current))) 
                       result) 
                 (mcdr current))))))
    
    (define (draw)
      (let loop ((graph-lines '())
                 (current (get-keys))
                 (color 1))
        (if (eq? '() current)
            (plot-bitmap graph-lines #:y-min 0 #:y-max 100)
            (loop (cons (lines (map-to-line (find (car current)))
                               #:label (car current)
                               #:color color)
                        graph-lines)
                  (cdr current)
                  (+ color 1)))))
    
    (define (dispatch message . args)
      (case message
        ((add!) (apply add! args))
        ((find) (apply find args))
        ((get-maps) (get-maps))
        ((get-keys) (get-keys))
        ((draw) (draw))))
    dispatch))

;(define (panel-graph prnt graph)
;  (define (list-callback button event)
;    (graph 'set-selected-map! (graph 'find (send button get-label))))
;  (define (graph-callback canvas dc)
;    (draw-graph graph dc))
;  (let ((panel (new horizontal-panel% [parent prnt])))
;    (panel-list panel (graph 'get-maps) list-callback)
;    (new canvas% [parent panel] [paint-callback graph-callback])))

(define (panel-graph prnt graph)
  (new message% [parent prnt] [label (graph 'draw)]))

