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
  (displayln (undetected-logical-error? result))
  
  ; Note: is it better to have the symbolic error as 3 single-qubit Paulis,
  ; or to have it as a 3-qubit Pauli, to take advantage of the bitvector representation?
  (define-symbolic-pauli1 err1)
  (define-symbolic-pauli1 err2)
  (define-symbolic-pauli1 err3)
  (define symb-result (run-circuit (repetition-circuit err1 err2 err3)))
  (define sol (solve (assert (undetected-logical-error? symb-result))))
  (if (unsat? sol)
      (printf "No logical error found\n")
      (begin
        (printf "Logical error found:\n")
        (define complete-sol (complete-solution sol (symbolics (list err1 err2 err3))))
        (define concrete-err1 (evaluate err1 complete-sol))
        (define concrete-err2 (evaluate err2 complete-sol))
        (define concrete-err3 (evaluate err3 complete-sol))
        (printf "~a; ~a; ~a\n"
          (pretty-pauli1 concrete-err1)
          (pretty-pauli1 concrete-err2)
          (pretty-pauli1 concrete-err3)
          )
      )
  ))
