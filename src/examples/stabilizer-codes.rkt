#lang rosette

(require "../paulis.rkt")

(provide (all-defined-out))

(define five-qubit-code
  (list (symbols->pauli '(X Z Z X I))
        (symbols->pauli '(I X Z Z X))
        (symbols->pauli '(X I X Z Z))
        (symbols->pauli '(Z X I X Z))))

(define tiny-example-code
  (list (symbols->pauli '(X Z Z X I))))

(define repetition-code
  (list (symbols->pauli '(Z Z I))
        (symbols->pauli '(I Z Z))))

(define shor-nine-qubit-code
  (list
    (symbols->pauli '(Z Z I I I I I I I))
    (symbols->pauli '(I Z Z I I I I I I))
    (symbols->pauli '(I I I Z Z I I I I))
    (symbols->pauli '(I I I I Z Z I I I))
    (symbols->pauli '(I I I I I I Z Z I))
    (symbols->pauli '(I I I I I I I Z Z))
    (symbols->pauli '(X X X X X X I I I))
    (symbols->pauli '(I I I X X X X X X))))

(define (bool->bv1 b)
  (if b (bv 1 1) (bv 0 1)))

;; Takes a two-bit syndrome and returns a three-qubit Pauli.
;; Construct bitvectors directly to support symbolic syndromes.
(define (repetition-code-decoder syndrome)
  ( unless (and (list? syndrome)
                (eq? (length syndrome) 2)
                (boolean? (first syndrome))
                (boolean? (second syndrome)))
      (raise-argument-error
        'repetition-code-decoder
        "ill-formed syndrome"
        syndrome))

  (define b0 (first syndrome))
  (define b1 (second syndrome))

  (define x0 (&& b0 (not b1)))
  (define x1 (&& b0 b1))
  (define x2 (&& (not b0) b1))

  (pauli 3
         (concat (bool->bv1 x0)
                 (bool->bv1 x1)
                 (bool->bv1 x2))
         (bv 0 3)))

  ; (let* ([b0 (first syndrome)]
  ;        [b1 (second syndrome)])
  ; (cond
  ;   [(and (not b0) (not b1)) => (symbols->pauli '(I I I))]
  ;   [(and (    b0) (not b1)) => (symbols->pauli '(X I I))]
  ;   [(and (not b0) (    b1)) => (symbols->pauli '(I I X))]
  ;   [(and (    b0) (    b1)) => (symbols->pauli '(I X I))]
  ; )))
