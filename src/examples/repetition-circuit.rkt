#lang rosette

(require "../circuits.rkt"
         "../paulis.rkt")

(provide (all-defined-out))

;; The three values specify faults on the three data qubits. If their X
;; components are x0, x1, and x2, the detector flips are x0 XOR x1 and
;; x1 XOR x2, while the observable flip is x0. Three X faults therefore give
;; no detection events but flip the logical observable.
(define (repetition-circuit p0 p1 p2)
  (circuit
   3
   (list (reset-qubit 0 'Z) (reset-qubit 1 'Z) (reset-qubit 2 'Z)
         (gate-fault 0 p0) (gate-fault 1 p1) (gate-fault 2 p2)
         (measure-qubit 0 'Z) (measure-qubit 1 'Z) (measure-qubit 2 'Z)
         (detector '(-3 -2))
         (detector '(-2 -1))
         (observable 0 '(-3)))))

(module+ main
  (define result (run-circuit (repetition-circuit pauli1X pauli1X pauli1X)))
  (displayln result)
  (displayln (undetected-logical-error? result)))
