#lang r5rs

(#%require "test-instruction.rkt"
           "test-parser.rkt"
           "test-element.rkt"
           "test-sensor.rkt"
           "test-actuator.rkt"
           "test-device.rkt"
           "test-steward.rkt"
           "test-time-interval.rkt"
           "test-recurrence.rkt"
           "test-rule.rkt"
           "test-hardware-device.rkt"
           "test-physical-room.rkt"
           "test-xbee-simulation.rkt"
           "test-zigbee.rkt")

(test-instruction)
(test-parser)
(test-element)
(test-sensor)
(test-actuator)
(test-device)
(test-steward)
(test-time-interval)
(test-recurrence)
(test-rule)
(test-hardware-device)
(test-physical-room)
(test-xbee-simulation)
(test-zigbee)

(display "Finished testing !")