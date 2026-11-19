#lang rosette

(require "../paulis/definitions.rkt")

;; This module tracks the fault-free circuit as a signed stabilizer state.
;; It validates that detector and observable parities are deterministic; the
;; separate Pauli frame in circuit.rkt computes their fault-induced flips.

(provide (all-defined-out))

;; An affine XOR expression: constant XOR the random bits selected by
;; random-mask. For example, #b101 selects r0 XOR r2. The constant may be a
;; symbolic Boolean from an intentional Pauli gate; the mask represents
;; independent measurement outcomes and is always concrete. XOR cancels
;; repeated random bits. An expression is nondeterministic exactly when its
;; random mask is nonzero, even if its constant is symbolic.
(struct parity (constant random-mask) #:transparent)
(define parity-zero (parity #f 0))
(define (bool-xor a b) (not (equal? a b)))
(define (parity-xor a b)
  (parity (bool-xor (parity-constant a) (parity-constant b))
          (bitwise-xor (parity-random-mask a) (parity-random-mask b))))
(define (parity-random? p) (not (zero? (parity-random-mask p))))

;; A row is a signed Pauli stabilizer generator:
;;
;;   (-1)^sign i^(x.z) X^x Z^z
;;
;; Bit q of x and z represents qubit q: (0,0)=I, (1,0)=X, (0,1)=Z,
;; (1,1)=Y. The i^(x.z) factor makes a joint X/Z bit represent Y, since
;; iXZ = Y. The sign is a parity because it can depend on previous random
;; measurements or symbolic intentional Paulis. Unlike a phase-free Pauli
;; frame, these rows retain signs. Reference holds the generators and the
;; index of the next fresh random measurement bit.
(struct row (x z sign) #:transparent)
(struct reference (width rows next-random) #:mutable)

(define (mask q) (arithmetic-shift 1 q))
(define (has? bits q) (bitwise-bit-set? bits q))
(define (toggle bits q) (bitwise-xor bits (mask q)))
(define (ones n)
  (let loop ([n n] [count 0])
    (if (zero? n) count
        (loop (bitwise-and n (sub1 n)) (add1 count)))))
;; The symplectic product counts X/Z crossings modulo two.
(define (anticommutes? a b)
  (odd? (+ (ones (bitwise-and (row-x a) (row-z b)))
           (ones (bitwise-and (row-z a) (row-x b))))))

;; Multiply commuting stabilizer rows, retaining the phase (e.g. XX*ZZ=-YY).
;; An odd power of i would mean the input rows did not commute.
(define (row-multiply a b)
  (define x (bitwise-xor (row-x a) (row-x b)))
  (define z (bitwise-xor (row-z a) (row-z b)))
  (define phase
    (modulo (+ (ones (bitwise-and (row-x a) (row-z a)))
               (ones (bitwise-and (row-x b) (row-z b)))
               (* 2 (ones (bitwise-and (row-z a) (row-x b))))
               (- (ones (bitwise-and x z))))
            4))
  (unless (even? phase)
    (error 'row-multiply "internal error: noncommuting stabilizer rows"))
  (row x z (parity-xor (parity-xor (row-sign a) (row-sign b))
                       (parity (= phase 2) 0))))

;; |0...0> is stabilized by Z on each qubit.
(define (make-reference width)
  (reference width
             (for/vector ([q (in-range width)])
               (row 0 (mask q) parity-zero))
             0))

(define (map-rows! ref f)
  (define rows (reference-rows ref))
  (for ([i (in-range (vector-length rows))])
    (vector-set! rows i (f (vector-ref rows i)))))

;; Gates conjugate each generator. H swaps X and Z and sends Y to -Y.
(define (reference-h! ref q)
  (map-rows!
   ref
   (lambda (r)
     (define x? (has? (row-x r) q))
     (define z? (has? (row-z r) q))
     (row (if (equal? x? z?) (row-x r) (toggle (row-x r) q))
          (if (equal? x? z?) (row-z r) (toggle (row-z r) q))
          (parity-xor (row-sign r) (parity (and x? z?) 0))))))

;; S sends X to Y and Y to -X.
(define (reference-s! ref q)
  (map-rows!
   ref
   (lambda (r)
     (define x? (has? (row-x r) q))
     (define z? (has? (row-z r) q))
     (row (row-x r)
          (if x? (toggle (row-z r) q) (row-z r))
          (parity-xor (row-sign r) (parity (and x? z?) 0))))))

;; CX propagates control X to target and target Z to control; the sign update
;; accounts for phases when both qubits carry Pauli components.
(define (reference-cx! ref c t)
  (map-rows!
   ref
   (lambda (r)
     (define xc (has? (row-x r) c))
     (define xt (has? (row-x r) t))
     (define zc (has? (row-z r) c))
     (define zt (has? (row-z r) t))
     (row (if xc (toggle (row-x r) t) (row-x r))
          (if zt (toggle (row-z r) c) (row-z r))
          (parity-xor (row-sign r)
                      (parity (and xc zt (equal? xt zc)) 0))))))

;; Conjugation by an intentional Pauli flips a generator's sign precisely
;; when they anticommute. Its Boolean exponents can themselves be symbolic.
(define (reference-pauli! ref q p)
  (map-rows!
   ref
   (lambda (r)
     (define flip
       (bool-xor (and (has? (row-x r) q) (pauli1-z p))
                 (and (has? (row-z r) q) (pauli1-x p))))
     (row (row-x r) (row-z r)
          (parity-xor (row-sign r) (parity flip 0))))))

(define (basis-row q basis)
  (case basis
    [(X) (row (mask q) 0 parity-zero)]
    [(Y) (row (mask q) (mask q) parity-zero)]
    [(Z) (row 0 (mask q) parity-zero)]))

(define (row-bits r width)
  (bitwise-ior (row-x r) (arithmetic-shift (row-z r) width)))

;; For a deterministic measurement, the observable is in the stabilizer span
;; of this pure reference state. GF(2) elimination finds its generator product;
;; the resulting sign is the outcome, possibly depending on earlier random
;; measurements. Row multiplication retains the signs during elimination.
(define (deterministic-sign ref measured)
  (define width (reference-width ref))
  (define basis (make-vector (* 2 width) #f))
  (for ([original (in-vector (reference-rows ref))])
    (let loop ([r original])
      (define bits (row-bits r width))
      (unless (zero? bits)
        (define pivot (sub1 (integer-length bits)))
        (define existing (vector-ref basis pivot))
        (if existing
            (loop (row-multiply r existing))
            (vector-set! basis pivot r)))))
  (let loop ([r measured])
    (define bits (row-bits r width))
    (if (zero? bits)
        (row-sign r)
        (let ([existing (vector-ref basis (sub1 (integer-length bits)))])
          (unless existing
            (error 'reference-measure! "internal error: observable not in stabilizer span"))
          (loop (row-multiply r existing))))))

;; If the measured Pauli anticommutes with a generator, its result is a fresh
;; random bit. Multiply the pivot into the other anticommuting rows so they
;; commute with the measurement, then replace the pivot with the measured
;; Pauli signed by that result. Otherwise the result comes from the existing
;; stabilizer span and no new random bit is needed.
;;
;; For example, Z measurements of a Bell pair can yield r0 and r0. Each is
;; random individually, but their XOR has an empty random mask and is a valid
;; deterministic detector parity.
(define (reference-measure! ref q basis)
  (define measured (basis-row q basis))
  (define rows (reference-rows ref))
  (define pivot
    (for/first ([r (in-vector rows)] [i (in-naturals)]
                #:when (anticommutes? r measured))
      i))
  (if pivot
      (let* ([old (vector-ref rows pivot)]
             [random-index (reference-next-random ref)]
             [result (parity #f (mask random-index))])
        (set-reference-next-random! ref (add1 random-index))
        (for ([i (in-range (vector-length rows))] #:unless (= i pivot))
          (define r (vector-ref rows i))
          (when (anticommutes? r measured)
            (vector-set! rows i (row-multiply r old))))
        (vector-set! rows pivot (row (row-x measured) (row-z measured) result))
        result)
      (deterministic-sign ref measured)))

;; Reset is an internal measurement followed by an anticommuting correction
;; conditioned on its outcome, producing the positive basis eigenstate.
;; The internal result is not appended to the circuit's measurement record;
;; correlations with other qubits can still depend on that random result.
(define (reference-reset! ref q basis)
  (define outcome (reference-measure! ref q basis))
  (define correction (basis-row q (if (eq? basis 'Z) 'X 'Z)))
  (map-rows!
   ref
   (lambda (r)
     (if (anticommutes? r correction)
         (row (row-x r) (row-z r) (parity-xor (row-sign r) outcome))
         r))))
