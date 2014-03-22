#lang racket
(require racket/gui/base)

(require "../structure/map.rkt")
(require "graph.rkt")

(provide panel-history-steward)

(define temp-map (new-map))
(temp-map 'add! 1 20)
(temp-map 'add! 2 20)
(temp-map 'add! 3 19)
(temp-map 'add! 4 18)
(temp-map 'add! 5 21)
(temp-map 'add! 6 20)
(temp-map 'add! 7 22)
(temp-map 'add! 8 19)
(temp-map 'add! 9 18)
(temp-map 'add! 10 21)
(define light-map (new-map))
(light-map 'add! 1 90)
(light-map 'add! 2 90)
(light-map 'add! 3 90)
(light-map 'add! 4 90)
(light-map 'add! 5 90)
(light-map 'add! 6 90)
(light-map 'add! 7 90)
(light-map 'add! 8 90)
(light-map 'add! 9 90)
(light-map 'add! 10 100)
(define test-graph (new-graph))
(test-graph 'add! "temperature" temp-map)
(test-graph 'add! "light" light-map)


(define (panel-history-steward prnt steward)
  ;DATA required from database
  ;now using test values
  (panel-graph prnt test-graph))