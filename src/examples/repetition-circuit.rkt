#lang rosette

(require "../circuits.rkt"
         "../paulis.rkt")

(provide (all-defined-out))

;; The three values specify faults on the three data qubits. If their X
;; components are x0, x1, and x2, the detector flips are x0 XOR x1 and
;; x1 XOR x2, while the observable flip is x0. Three X faults therefore give
;; no detection events but flip the logical observable.
(define (repetition-circuit err)
  (circuit
   3
   (list (reset-qubit 0 'Z) (reset-qubit 1 'Z) (reset-qubit 2 'Z)
         (gate-fault 0 (pauli1-at 0 err)) (gate-fault 1 (pauli1-at 1 err)) (gate-fault 2 (pauli1-at 2 err))
         (measure-qubit 0 'Z) (measure-qubit 1 'Z) (measure-qubit 2 'Z)
         (detector '(-3 -2))
         (detector '(-2 -1))
         (observable 0 '(-3)))))

  

(module+ main
  ;(define result (run-circuit (repetition-circuit pauli1X pauli1X pauli1X)))
  ;(displayln result)
  ;(displayln (undetected-logical-error? result))
  
  ; Note: is it better to have the symbolic error as 3 single-qubit Paulis,
  ; or to have it as a 3-qubit Pauli, to take advantage of the bitvector representation?
  ;(define-symbolic-pauli err 3)
  ;(pretty-pauli (find-circuit-logical-error (repetition-circuit err) err))
  (print-paulis (find-circuit-error-generators 3 repetition-circuit))
)
