#lang rosette

(require rackunit
         racket/match
         racket/runtime-path
         "../src/paulis.rkt"
         "../src/stabilizer-codes.rkt"
         "../src/circuits.rkt"
         "../main.rkt"
         (only-in "../src/paulis/definitions.rkt" bool-at)
         (only-in "../src/paulis/algebra.rkt" bv-inner-product)
         (only-in "../src/paulis/sampling.rkt" sample-bool)
         (only-in "../src/stabilizer-codes/logical.rkt" wf-decoder?)
         (only-in "../src/circuits/circuit.rkt" record-parity)
         (prefix-in reference: "../src/circuits/reference.rkt"))

(define-runtime-path paulis-path "../src/paulis.rkt")
(define-runtime-path stabilizer-codes-path "../src/stabilizer-codes.rkt")
(define-runtime-path circuits-path "../src/circuits.rkt")
(define-runtime-path main-path "../main.rkt")

(test-case "domain and aggregate interfaces share usable bindings"
  (check-equal? (match (pauli1 #t #f) [(pauli1 x z) (list x z)])
                '(#t #f))
  (define-symbolic-pauli symbolic-pauli 2)
  (check-true (symbolic? (pauli-xs symbolic-pauli)))
  (define zz (symbols->pauli '(Z Z)))
  (define xx (symbols->pauli '(X X)))
  (check-equal? (sample-group (list zz) #:allow-empty #f) zz)
  (check-true (logical-error? xx (list zz)))
  (check-not-false (member (pauli->symbols (sample-logical-error (list zz) (list xx)))
                           '((X X) (Y Y))))
  (define result (run-circuit (circuit 1 (list (measure-qubit 0 'Z)))))
  (check-equal? (circuit-result-detectors result) '()))

(test-case "implementation helpers remain accessible but not public"
  (check-true (procedure? bool-at))
  (check-true (procedure? bv-inner-product))
  (check-true (procedure? sample-bool))
  (check-true (procedure? wf-decoder?))
  (check-true (procedure? record-parity))
  (check-true (reference:row? (reference:row 0 0 #f)))
  (for ([entry (list (cons paulis-path 'bool-at)
                     (cons paulis-path 'sample-bool)
                     (cons stabilizer-codes-path 'wf-decoder?)
                     (cons stabilizer-codes-path 'five-qubit-code)
                     (cons circuits-path 'record-parity)
                     (cons circuits-path 'make-reference)
                     (cons main-path 'example1)
                     (cons main-path 'run-demo))])
    (check-exn exn:fail?
               (lambda () (dynamic-require (car entry) (cdr entry))))))
