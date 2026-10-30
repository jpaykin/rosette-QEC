#lang racket/base

(require rackunit
         rosette
         "../main.rkt"
         "./pauli.rkt"
         "./algebra.rkt")


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

(test-case "define-symbolic-syndrome creates symbolic boolean bits"
  (define syndrome (define-symbolic-syndrome 3))
  (check-equal? (length syndrome) 3)
  (check-true (andmap (lambda (bit)
                        (and (boolean? bit) (symbolic? bit)))
                      syndrome))
  (check-equal? (define-symbolic-syndrome 0) '())
  (check-exn exn:fail:contract?
             (lambda () (define-symbolic-syndrome -1))))
