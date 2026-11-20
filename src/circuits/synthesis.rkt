#lang rosette

(require "../paulis.rkt"
         "circuit.rkt")

(provide (all-defined-out))

; Define a pauli-property? for finding circuit logical errors
(define (circuit-logical-error? circ) (lambda (P #:except grp)
    (and 
        (undetected-logical-error-result? (run-circuit (circ P)))
        (not (pauli-in-group? P grp))
    )
))

; circ is a function from a Pauli (storing all the faults of the circuit)
; to a circuit.
; r is the number of faults in the circuit
(define (find-circuit-logical-error r circ)
    (synthesize-pauli r (circuit-logical-error? circ))
)

(define (find-circuit-error-generators r circ)
    (synthesize-generators r (circuit-logical-error? circ))
)