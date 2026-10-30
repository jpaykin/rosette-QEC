#lang racket/base

(require rackunit
         "../src/ler/algebra.rkt"
         "../src/ler/pauli.rkt")

(test-case "syndrome-of marks generators that anticommute"
  (define generators
    (list (symbols->pauli '(Z I))
          (symbols->pauli '(I Z))))

  (check-equal? (syndrome-of (symbols->pauli '(I I)) generators)
                '(#f #f))
  (check-equal? (syndrome-of (symbols->pauli '(X I)) generators)
                '(#t #f))
  (check-equal? (syndrome-of (symbols->pauli '(I X)) generators)
                '(#f #t))
  (check-equal? (syndrome-of (symbols->pauli '(X X)) generators)
                '(#t #t)))
