#lang rosette

(require racket/match
         "pauli.rkt"
         "circuit-reference.rkt")

(provide (struct-out circuit)
         (struct-out gate-h)
         (struct-out gate-s)
         (struct-out gate-cx)
         (struct-out gate-pauli)
         (struct-out gate-fault)
         (struct-out measure-qubit)
         (struct-out reset-qubit)
         (struct-out detector)
         (struct-out observable)
         (struct-out circuit-result)
         run-circuit
         undetected-logical-error?)

(struct circuit (width instructions) #:transparent)
(struct gate-h (qubit) #:transparent)
(struct gate-s (qubit) #:transparent)
(struct gate-cx (control target) #:transparent)
(struct gate-pauli (qubit pauli) #:transparent)
(struct gate-fault (qubit pauli) #:transparent)
(struct measure-qubit (qubit basis) #:transparent)
(struct reset-qubit (qubit basis) #:transparent)
(struct detector (records) #:transparent)
(struct observable (index records) #:transparent)
;; Results are flips relative to the fault-free circuit, not raw measurements.
(struct circuit-result (detectors observables) #:transparent)

(define (bool-xor a b) (not (equal? a b)))

(define (check-qubit q width)
  (unless (and (concrete? q) (exact-nonnegative-integer? q) (< q width))
    (raise-arguments-error 'run-circuit "invalid qubit index"
                           "qubit" q "circuit width" width)))

(define (check-basis basis)
  (unless (and (concrete? basis) (memq basis '(X Y Z)))
    (raise-argument-error 'run-circuit "(or/c 'X 'Y 'Z)" basis)))

(define (check-pauli p)
  (unless (single-qubit-pauli? p)
    (raise-argument-error 'run-circuit "single-qubit-pauli?" p)))

;; Records are newest-first, so -1 selects the latest measurement at this
;; instruction. Repeated offsets cancel under XOR.
(define (record-parity offsets records zero combine)
  (unless (list? offsets)
    (raise-argument-error 'run-circuit "list of negative record offsets" offsets))
  (for/fold ([result zero]) ([offset (in-list offsets)])
    (unless (and (concrete? offset) (exact-integer? offset)
                 (< offset 0) (<= (- offset) (length records)))
      (raise-arguments-error 'run-circuit "invalid measurement record offset"
                             "offset" offset "measurements so far" (length records)))
    (combine result (list-ref records (sub1 (- offset))))))

;; Execute the ideal reference and a fault frame in parallel. The reference
;; checks noiseless parity determinism; xs and zs describe the Pauli error on
;; each qubit and can contain symbolic Booleans. Neither computation samples
;; measurements or searches for faults.
(define (run-circuit c)
  (unless (circuit? c)
    (raise-argument-error 'run-circuit "circuit?" c))
  (define width (circuit-width c))
  (unless (and (concrete? width) (exact-nonnegative-integer? width))
    (raise-argument-error 'run-circuit "concrete exact-nonnegative-integer? width" width))
  (unless (list? (circuit-instructions c))
    (raise-argument-error 'run-circuit "list of circuit instructions" (circuit-instructions c)))
  (define ref (make-reference width))
  (define xs (make-vector width #f))
  (define zs (make-vector width #f))
  (define reference-records '())
  (define fault-records '())
  (define detector-flips '())
  (define reference-observables (make-hash))
  (define fault-observables (make-hash))
  (define max-observable -1)
  (for ([instruction (in-list (circuit-instructions c))]
        [position (in-naturals)])
    (match instruction
      [(gate-h q)
       (check-qubit q width)
       (reference-h! ref q)
       (define x (vector-ref xs q))
       (vector-set! xs q (vector-ref zs q))
       (vector-set! zs q x)]
      [(gate-s q)
       (check-qubit q width)
       (reference-s! ref q)
       (vector-set! zs q (bool-xor (vector-ref zs q) (vector-ref xs q)))]
      [(gate-cx c t)
       (check-qubit c width)
       (check-qubit t width)
       (when (= c t)
         (raise-arguments-error 'run-circuit "CX control and target must differ" "qubit" c))
       (reference-cx! ref c t)
       (vector-set! xs t (bool-xor (vector-ref xs t) (vector-ref xs c)))
       (vector-set! zs c (bool-xor (vector-ref zs c) (vector-ref zs t)))]
      [(gate-pauli q p)
       (check-qubit q width)
       (check-pauli p)
       ;; An intentional Pauli belongs to the ideal reference. Conjugating
       ;; the fault frame by a Pauli changes only its irrelevant global phase.
       (reference-pauli! ref q p)]
      [(gate-fault q p)
       (check-qubit q width)
       (check-pauli p)
       ;; A fault is identity in the reference and is XORed into the frame.
       (vector-set! xs q (bool-xor (vector-ref xs q) (pauli1-x p)))
       (vector-set! zs q (bool-xor (vector-ref zs q) (pauli1-z p)))]
      [(measure-qubit q basis)
       (check-qubit q width)
       (check-basis basis)
       (set! reference-records (cons (reference-measure! ref q basis) reference-records))
       ;; The measurement flips exactly when its Pauli anticommutes with the
       ;; frame: X reads z, Z reads x, and Y reads x XOR z.
       (define flip
         (case basis
           [(X) (vector-ref zs q)]
           [(Z) (vector-ref xs q)]
           [(Y) (bool-xor (vector-ref xs q) (vector-ref zs q))]))
       (set! fault-records (cons flip fault-records))]
      [(reset-qubit q basis)
       (check-qubit q width)
       (check-basis basis)
       (reference-reset! ref q basis)
       ;; Reset removes the frame on this qubit, not on other qubits.
       (vector-set! xs q #f)
       (vector-set! zs q #f)]
      [(detector offsets)
       ;; First verify ideal determinism, then compute the corresponding flip.
       (define reference-parity
         (record-parity offsets reference-records parity-zero parity-xor))
       (when (parity-random? reference-parity)
         (raise-arguments-error 'run-circuit "nondeterministic detector"
                                "instruction index" position
                                "detector index" (length detector-flips)))
       (set! detector-flips
             (cons (record-parity offsets fault-records #f bool-xor) detector-flips))]
      [(observable index offsets)
       (unless (and (concrete? index) (exact-nonnegative-integer? index))
         (raise-argument-error 'run-circuit "concrete exact-nonnegative-integer? observable index" index))
       (set! max-observable (max index max-observable))
       ;; Multiple annotations for one ID contribute by XOR. Validation must
       ;; wait until all contributions have been accumulated.
       (hash-set! reference-observables index
                  (parity-xor (hash-ref reference-observables index parity-zero)
                              (record-parity offsets reference-records parity-zero parity-xor)))
       (hash-set! fault-observables index
                  (bool-xor (hash-ref fault-observables index #f)
                            (record-parity offsets fault-records #f bool-xor)))]
      [_
       (raise-arguments-error 'run-circuit "unsupported circuit instruction"
                              "instruction index" position "instruction" instruction)]))
  (for ([(index p) (in-hash reference-observables)])
    (when (parity-random? p)
      (raise-arguments-error 'run-circuit "nondeterministic completed observable"
                             "observable index" index)))
  (circuit-result (reverse detector-flips)
                  (for/list ([index (in-range (add1 max-observable))])
                    (hash-ref fault-observables index #f))))

;; An undetected logical error flips at least one observable and no detectors.
(define (undetected-logical-error? result)
  (unless (circuit-result? result)
    (raise-argument-error 'undetected-logical-error? "circuit-result?" result))
  (and (for/fold ([none #t]) ([d (in-list (circuit-result-detectors result))])
         (and none (not d)))
       (for/fold ([any #f]) ([o (in-list (circuit-result-observables result))])
         (or any o))))