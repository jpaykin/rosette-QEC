#lang racket/base

(require rackunit
         rosette
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

(test-case "bits->pauli constructs X_i^bx Z_i^bz"
  (for* ([bx (in-list '(#f #t))]
         [bz (in-list '(#f #t))])
    (define P (bits->pauli 3 1 bx bz))
    (check-equal? (pauli->symbols P)
                  (list 'I
                        (cond [(and bx bz) 'Y]
                              [bx 'X]
                              [bz 'Z]
                              [else 'I])
                        'I)))
  (check-exn exn:fail:contract? (lambda () (bits->pauli 3 3 #t #f)))
  (check-exn exn:fail:contract? (lambda () (bits->pauli 3 1 1 #f))))

(test-case "bits->pauli accepts symbolic Boolean exponents"
  (define-symbolic bx bz boolean?)
  (define P (bits->pauli 3 1 bx bz))
  (check-true (symbolic? (pauli-xs P)))
  (check-true (symbolic? (pauli-zs P))))
