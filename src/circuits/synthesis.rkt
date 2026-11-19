#lang rosette

(require "../paulis.rkt"
         "circuit.rkt")

(provide (all-defined-out))

; circ is a function from a Pauli (storing all the faults of the circuit)
; to a circuit.
; r is the number of faults in the circuit
(define (find-circuit-logical-error r circ)
    (define (prop P) (undetected-logical-error? (run-circuit (circ P))))
    (synthesize-pauli r prop)
)

(define (find-circuit-error-generators r circ)
    (define (prop P) (undetected-logical-error? (run-circuit (circ P))))
    (synthesize-generators r prop)
)