#lang racket/base

(require rackunit
         "../src/ler/pauli.rkt")

(test-case "Pauli symbol conversion round-trips"
  (define symbols '(I X Z Y))
  (define P (symbols->pauli symbols))
  (check-equal? (pauli->symbols P) symbols)
  (check-equal? (pretty-pauli P) "IXZY")
  )

(test-case "Pauli Identity"
  (define P (pauli-id 4))
  (check-equal? (pauli->symbols P) '(I I I I))
  (check-equal? (pretty-pauli P) "IIII")
  )


(test-case "symbol->pauli"
  (define P (symbol->pauli 1 'X 0))
  (check-equal? (pauli->symbols P) '(X))
  )


(test-case "Single-symbol Pauli is placed at the requested bit index"
  (for ([symbol (in-list '(I X Z Y))])
    (define P (symbol->pauli 3 symbol 1))
    (check-equal? (pauli1-at 1 P) (symbol->pauli1 symbol))
    (check-equal? (pauli->symbols P)
                  (case symbol
                    [(I) '(I I I)]
                    [(X) '(I X I)]
                    [(Z) '(I Z I)]
                    [(Y) '(I Y I)])))
  (check-exn exn:fail:contract? (lambda () (symbol->pauli 2 'X 2))))

