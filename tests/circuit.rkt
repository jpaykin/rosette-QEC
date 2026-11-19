#lang rosette

(require rackunit
         "../src/circuits/circuit.rkt"
         "../src/paulis/definitions.rkt"
         "../src/examples/repetition-circuit.rkt")

(define paulis (list pauli1I pauli1X pauli1Y pauli1Z))
(define (run width . instructions)
  (run-circuit (circuit width instructions)))
(define (xor a b) (not (equal? a b)))
(define (fault-pauli p0 p1 p2)
  (symbols->pauli (map pauli1->symbol (list p2 p1 p0))))

(test-case "exhaustive repetition-code faults"
  (define logical-count 0)
  (for* ([p0 (in-list paulis)] [p1 (in-list paulis)] [p2 (in-list paulis)])
    (define result (run-circuit (repetition-circuit (fault-pauli p0 p1 p2))))
    (define x0 (pauli1-x p0))
    (define x1 (pauli1-x p1))
    (define x2 (pauli1-x p2))
    (check-equal? (circuit-result-detectors result) (list (xor x0 x1) (xor x1 x2)))
    (check-equal? (circuit-result-observables result) (list x0))
    (when (undetected-logical-error-result? result)
      (set! logical-count (add1 logical-count))))
  (check-equal? logical-count 8))

(test-case "symbolic faults agree with all concrete assignments"
  (define-symbolic xs zs (bitvector 3))
  (define result
    (run-circuit (repetition-circuit (pauli 3 xs zs))))
  (for* ([p0 (in-list paulis)] [p1 (in-list paulis)] [p2 (in-list paulis)])
    (define fault (fault-pauli p0 p1 p2))
    (define solution
      (sat (hash xs (pauli-xs fault) zs (pauli-zs fault))))
    (define concrete (run-circuit (repetition-circuit fault)))
    (check-equal? (evaluate result solution) concrete)
    (check-equal? (evaluate (undetected-logical-error-result? result) solution)
                  (undetected-logical-error-result? concrete))))

(test-case "stabilizer results have no detector or observable flips"
  (check-true (circuit-stabilizer-result? (circuit-result '() '())))
  (check-true (circuit-stabilizer-result? (circuit-result '(#f #f) '(#f #f))))
  (check-false (circuit-stabilizer-result? (circuit-result '(#f #t) '(#f))))
  (check-false (circuit-stabilizer-result? (circuit-result '(#f) '(#f #t))))
  (check-false (circuit-stabilizer-result? (circuit-result '(#t) '(#t))))
  (check-exn exn:fail:contract? (lambda () (circuit-stabilizer-result? 'not-a-result)))
  (define-symbolic detector-flip observable-flip boolean?)
  (define symbolic-result (circuit-result (list detector-flip) (list observable-flip)))
  (for* ([d '(#f #t)] [o '(#f #t)])
    (define solution (sat (hash detector-flip d observable-flip o)))
    (check-equal? (evaluate (circuit-stabilizer-result? symbolic-result) solution)
                  (and (not d) (not o))))
  (define (circuit-with-faults d o)
    (run 2 (gate-fault 0 (pauli1 d #f)) (gate-fault 1 (pauli1 o #f))
         (measure-qubit 0 'Z) (detector '(-1))
         (measure-qubit 1 'Z) (observable 0 '(-1))))
  (define symbolic-circuit-result (circuit-with-faults detector-flip observable-flip))
  (for* ([d '(#f #t)] [o '(#f #t)])
    (define solution (sat (hash detector-flip d observable-flip o)))
    (check-equal? (evaluate (circuit-stabilizer-result? symbolic-circuit-result) solution)
                  (circuit-stabilizer-result? (circuit-with-faults d o)))))

(test-case "ideal Pauli and fault Pauli have distinct reference semantics"
  (define (after constructor p)
    (run 1 (constructor 0 p) (measure-qubit 0 'Z) (detector '(-1))
         (observable 0 '(-1))))
  (check-equal? (after gate-pauli pauli1X) (circuit-result '(#f) '(#f)))
  (check-equal? (after gate-fault pauli1X) (circuit-result '(#t) '(#t)))
  (define-symbolic x z boolean?)
  (define ideal (after gate-pauli (pauli1 x z)))
  (define fault (after gate-fault (pauli1 x z)))
  (for ([p (in-list paulis)])
    (define sol (sat (hash x (pauli1-x p) z (pauli1-z p))))
    (check-equal? (evaluate ideal sol) (after gate-pauli p))
    (check-equal? (evaluate fault sol) (after gate-fault p))))

(test-case "H, S and CX propagate faults"
  (check-equal?
   (run 1 (gate-fault 0 pauli1X) (gate-h 0)
        (measure-qubit 0 'X) (detector '(-1)))
   (circuit-result '(#t) '()))
  (check-equal?
   (run 1 (gate-h 0) (gate-fault 0 pauli1X) (gate-s 0)
        (measure-qubit 0 'Y) (detector '(-1)))
   (circuit-result '(#f) '()))
  (check-equal?
   (run 1 (gate-h 0) (gate-fault 0 pauli1Z) (gate-s 0)
        (measure-qubit 0 'Y) (detector '(-1)))
   (circuit-result '(#t) '()))
  (check-equal?
   (run 2 (gate-fault 0 pauli1Y) (gate-cx 0 1)
        (measure-qubit 0 'Z) (measure-qubit 1 'Z)
        (detector '(-2)) (detector '(-1)))
   (circuit-result '(#t #t) '()))
  (check-equal?
   (run 2 (gate-h 0) (gate-h 1) (gate-fault 1 pauli1Z) (gate-cx 0 1)
        (gate-h 0) (measure-qubit 0 'Z) (detector '(-1)))
   (circuit-result '(#t) '())))

(test-case "basis measurements, repeated measurements and resets"
  (for ([basis '(X Y Z)] [commuting (list pauli1X pauli1Y pauli1Z)]
        [anticommuting (list pauli1Z pauli1X pauli1X)])
    (check-equal?
     (run 1 (reset-qubit 0 basis) (gate-fault 0 commuting)
          (measure-qubit 0 basis) (detector '(-1)))
     (circuit-result '(#f) '()))
    (check-equal?
     (run 1 (reset-qubit 0 basis) (gate-fault 0 anticommuting)
          (measure-qubit 0 basis) (measure-qubit 0 basis)
          (detector '(-2)) (detector '(-1 -2))
          (reset-qubit 0 basis) (measure-qubit 0 basis) (detector '(-1)))
     (circuit-result '(#t #f #f) '()))))

(test-case "random measurements may have deterministic correlated parities"
  (check-equal?
   (run 2 (gate-h 0) (gate-cx 0 1)
        (measure-qubit 0 'Z) (measure-qubit 1 'Z)
        (detector '(-1 -2))
        (observable 0 '(-1)) (observable 0 '(-2)))
   (circuit-result '(#f) '(#f)))
  (check-exn #rx"nondeterministic detector"
             (lambda () (run 1 (gate-h 0) (measure-qubit 0 'Z) (detector '(-1)))))
  (check-exn #rx"nondeterministic completed observable"
             (lambda () (run 1 (gate-h 0) (measure-qubit 0 'Z) (observable 0 '(-1)))))
  (check-exn #rx"nondeterministic detector"
             (lambda ()
               (run 2 (gate-h 0) (gate-h 1) (measure-qubit 0 'Z)
                    (measure-qubit 1 'Z) (detector '(-1 -2))))))

(test-case "reset of an entangled qubit preserves correlations in the reference"
  (check-equal?
   (run 2 (gate-h 0) (gate-cx 0 1) (gate-fault 0 pauli1X)
        (measure-qubit 0 'Z) (reset-qubit 0 'Z) (measure-qubit 1 'Z)
        (detector '(-1 -2)))
   (circuit-result '(#t) '()))
  (check-exn #rx"nondeterministic detector"
             (lambda ()
               (run 2 (gate-h 0) (gate-cx 0 1) (reset-qubit 0 'Z)
                    (measure-qubit 1 'Z) (detector '(-1))))))

(test-case "observable indexing and repeated references are XORed"
  (check-equal?
   (run 1 (gate-fault 0 pauli1X) (measure-qubit 0 'Z)
        (detector '(-1 -1)) (observable 2 '(-1))
        (observable 2 '(-1)) (observable 1 '(-1)))
   (circuit-result '(#f) '(#f #t #f))))

(test-case "two-round ancilla circuit"
  (check-equal?
   (run 3 (gate-cx 0 2) (gate-cx 1 2)
        (measure-qubit 2 'Z) (detector '(-1)) (reset-qubit 2 'Z)
        (gate-fault 0 pauli1X)
        (gate-cx 0 2) (gate-cx 1 2)
        (measure-qubit 2 'Z) (detector '(-1 -2))
        (measure-qubit 0 'Z) (observable 0 '(-1)))
   (circuit-result '(#f #t) '(#t))))

(test-case "invalid circuits are rejected"
  (for ([thunk (in-list
                (list (lambda () (run 1 (gate-h 1)))
                      (lambda () (run 1 (gate-cx 0 0)))
                      (lambda () (run 1 (gate-pauli 0 (pauli1 1 #f))))
                      (lambda () (run 1 (gate-fault 0 'X)))
                      (lambda () (run 1 (measure-qubit 0 'Q)))
                      (lambda () (run 1 (detector '(-1))))
                      (lambda () (run 1 (measure-qubit 0 'Z) (detector '(0))))
                      (lambda () (run 1 (observable -1 '())))
                      (lambda () (run 1 'unknown))))])
    (check-exn exn:fail? thunk)))
