#lang r5rs
(#%provide new-rule-manager)
(#%require (only racket/base sleep))
(#%require (only racket/base thread))

(define sleep-time 1)

;b===c* rules/rule-manager
; NAME
;  rule-manager
; DESCRIPTION
;  Een manager die rules beheerst.
;  Elke steward heeft zijn eigen rule-manager.
;e===
;b===o* rule-manager/new-rule-manager
; NAME
;  new-rule-manager
; DESCRIPTION
;  Maakt een nieuw rule-manager object aan.
; PARAMETERS
;  * steward - de steward die deze rule-manager beheerst.
; SYNOPSIS
(define (new-rule-manager steward)
;e===
  (let ((rules '()))
    ;b===m* rule-manager/get-steward
    ; NAME
    ;  get-steward
    ; DESCRIPTION
    ;  Geeft de steward die deze manager beheerst terug.
    ; RETURN VALUE
    ;  steward - de steward van deze manager.
    ; SYNOPSIS
    (define (get-steward)
    ; SOURCE
      steward)
    ;e===
    ;b===m* rule-manager/add-rule
    ; NAME
    ;  add-rule
    ; DESCRIPTION
    ;  Voeg een nieuwe rule toe aan deze manager.
    ; PARAMETERS
    ;  * rule - de rule die toegevoegd moet worden.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (add-rule rule)
    ; SOURCE
      (set! rules (cons rule rules)))
    ;e===
    ;b===m* rule-manager/get-rules
    ; NAME
    ;  get-rules
    ; DESCRIPTION
    ;  Geeft een list met de rules die deze rule-manager beheerst terug.
    ; RETURN VALUE
    ;  list - een list met de rules van deze manager.
    ; SYNOPSIS
    (define (get-rules) 
    ; SOURCE
      rules)
    ;e===
    ;b===m* rule-manager/remove-rule
    ; NAME
    ;  remove-rule
    ; DESCRIPTION
    ;  Verwijder een rule van deze rule-manager.
    ; PARAMETERS
    ;  * rule - de rule die verwijdert moet worden.
    ; RETURN VALUE
    ;  #f - false wanneer de rule niet gevonden werd.
    ;  #t - true wanneer de rule verwijderd werd.
    ; SYNOPSIS
    (define (remove-rule rule)
    ; SOURCE
      (define (loop current previous)
        (cond
          ((eq? '() current) #f) ;rule not in list
          ((eq? rule (car current))
           (if (eq? '() previous)
               (set! rules (cdr current))
               (set-cdr! previous (cdr current)))
           #t) ;found and removed rule
          (else (loop (cdr current) current))))
      (loop rules '()))
    ;e===
    ;b===m* rule-manager/execute
    ; NAME
    ;  execute
    ; DESCRIPTION
    ;  Probeer alle rules van deze manager toe te passen op zijn steward.
    ; RETURN VALUE
    ;  #<void>
    ; SYNOPSIS
    (define (execute)
    ; SOURCE
      ;(display "executing for ") (display (steward 'get-room)) (newline)
      (for-each (lambda (r) (r 'execute steward)) rules)
      ;(display "executed for ") (display (steward 'get-room)) (newline)
      )
    ;e===
    
    (define (dispatch message . args)
      (case message
        ((get-steward) (get-steward))
        ((get-rules) (get-rules))
        ((add-rule) (apply add-rule args))
        ((remove-rule) (apply remove-rule args))
        ((execute) (execute))))
    
    dispatch))