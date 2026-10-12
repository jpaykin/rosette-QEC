#lang racket/base

(require rackunit
         "../main.rkt"
         "./pauli.rkt")


(test-case "symbol->pauli"
  (define P (pauli-mult (symbol->pauli 4 'X 2) (symbol->pauli 4 'Z 3)))
  (check-equal? (pauli->symbols P) '(I I X Z))

  (define P0 (pauli-mult (symbol->pauli 4 'X 2) (symbol->pauli 4 'Y 2)))
  (check-equal? (pauli->symbols P0) '(I I Z I))
  )


(test-case "Pauli multiplication is XOR of components"
  (check-equal? (pauli->symbols
                 (pauli-mult (symbols->pauli '(X))
                             (symbols->pauli '(X))))
                '(I)))


(test-case "Commutation, group membership, and logical errors"
  (define xx (symbols->pauli '(X X)))
  (define zz (symbols->pauli '(Z Z)))
  (check-true (commute? xx zz))
  (check-true (pauli-in-group? xx (list xx)))
  (check-false (pauli-in-group? zz (list xx)))
  (check-true (logical-error? zz (list xx)))
  (check-false (logical-error? xx (list xx))))
