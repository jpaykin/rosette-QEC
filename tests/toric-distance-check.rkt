
#lang rosette

(require "../src/ler/pauli.rkt"
         "../src/ler/algebra.rkt"
         "../src/ler/logical.rkt"
         "../src/ler/examples/toric-code.rkt")

(provide verify-toric-distance)

;; Count the nonidentity positions of a Pauli operator.
;; this is the weight of the Pauli operator.
; (define (weight-of P)
;   (for/sum ([i (in-range (pauli-width P))])
;     (define p (pauli1-at i P))
;     (if (or (pauli1-x p) (pauli1-z p))
;         1
;         0)))

;; Construct a weight-L noncontractible logical operator.
;; Q = X_0 X_L X_2L ... X_((L-1)L)
(define (weight-L-logical L n)
  (for/fold ([P (pauli-id n)])
            ([row (in-range L)])
    (pauli-mult P
                (bits->pauli n (* row L) #t #f))))

;; Verify that the L x L toric code has distance d = L.
(define (verify-toric-distance L)
;; Check that L is a valid input.
  (unless (and (exact-integer? L) (>= L 3))
    (raise-argument-error
     'verify-toric-distance "exact integer >= 3" L))

  ;; Generate the toric code.
  ;; The toric code has 2L^2 stabilizer generators and 2L^2 physical qubits.
  (define n (* 2 L L))
  ;; G is a list of stabilizer generators for the toric code.
  ;;toric-code generates the stabilizer generators for the toric code of size L.
  (define G (toric-code L))

  (printf "Checking L=~a toric code\n" L)
  (printf "Physical qubits: ~a\n" n)
  (printf "Stabilizer generators: ~a\n" (length G))

  ;; Verify that the number of stabilizer generators is correct.
  (unless (= (length G) (* 2 L L))
    (error 'verify-toric-distance
           "unexpected number of stabilizer generators"))

  ;; Upper bound: construct a logical operator of weight L.
  (define Q (weight-L-logical L n))

  (unless (and (= (pauli-weight Q) L)
               (logical-error? Q G))
    (error 'verify-toric-distance
           "constructed operator is not a weight-L logical operator"))

  (printf "PASS: Weight-~a logical operator exists (d <= ~a)\n"
          L L)

  ;; Lower bound: search for a logical error of weight < L.
  ;; Fresh symbolic variables permit repeated function calls.
  ;; xs and zs are symbolic bitvectors of length n.
  ;; E is a symbolic Pauli operator of width n.
  
  (define-symbolic* xs (bitvector n))
  (define-symbolic* zs (bitvector n))
  (define E (pauli n xs zs))
  
  ;; Check if E is a logical error of weight < L.
  (define result
    (solve
      (assert
        (and (logical-error? E G)
             (< (pauli-weight E) L)))))

  ;; Display the verification result.
  (cond
    [(unsat? result)
     (printf "PASS: No logical errors of weight < ~a\n" L)
     (printf "VERIFIED: L=~a toric code has distance d=~a\n"
             L L)
     #t]

    [(sat? result)
     (define found (evaluate E result))
     (displayln "FAIL: Found a smaller logical error:")
     (displayln (pretty-pauli found))
     (printf "Weight: ~a\n" (pauli-weight found))
     (error 'verify-toric-distance
            "distance lower bound failed")]

    [else
     (error 'verify-toric-distance
            "UNKNOWN: Solver could not decide")]))

;; Run from the command line.
;; Example: racket distance-check.rkt 4
(module+ main
  (define args
    (vector->list (current-command-line-arguments)))

  (unless (= (length args) 1)
    (error 'distance-check
           "usage: racket distance-check.rkt <L>"))

  (verify-toric-distance
   (string->number (first args))))

;; Run with: raco test distance-check.rkt
(module+ test
  (require rackunit)
  (check-true (verify-toric-distance 3)))
