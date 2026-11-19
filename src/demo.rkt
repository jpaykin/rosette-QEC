#lang rosette

(require "paulis.rkt"
         "stabilizer-codes.rkt"
         "examples/toric-code.rkt"
         "circuits.rkt"
         "examples/stabilizer-codes.rkt"
         )

(provide (all-defined-out))

(define (run-demo)
  ;(displayln (pretty-pauli1 (pauli1 #f #t)))
  ;(probability-weight-given-logical-error 3 five-qubit-code 1000)
  ; (define symbolic-synd (define-symbolic-syndrome 2))
  ; (displayln symbolic-synd)
  ; (displayln (repetition-code-decoder symbolic-synd))

  ; (decoder-logical-error?
  ;   (symbols->pauli '(X X X))
  ;   repetition-code-decoder
  ;   repetition-code
  ;   '()
  ;   )

  ; (define P (symbols->pauli '(X X X)))
  ; (println (syndrome-of P repetition-code))

  ; (pauli->symbols
  ;   (pauli-mult
  ;     (repetition-code-decoder (syndrome-of P repetition-code))
  ;     P))

  ; (println (decoder-logical-error? P 
  ;   repetition-code-decoder
  ;   repetition-code
  ;   '()))

  ;(pauli->symbols (find-decoder-logical-error repetition-code-decoder repetition-code))


  (printf "Generators of logical-errors for repetition code:\n")
  (print-paulis 
    (find-logical-error-generators
      repetition-code
    ))
  (printf "\n")


  (printf "Generators of decoder-logical-errors for repetition code:\n")
  (print-paulis 
    (find-decoder-logical-error-generators
      repetition-code-decoder
      repetition-code
    ))
  (printf "\n")


  (printf "Generators of the logical errors of the L3 toric code:\n")
  (define generators (toric-code 3))
  ; ;(find-logical-error generators)
  (define error-generators (find-logical-error-generators generators))
  (print-paulis error-generators)


  ; (printf "Number of stabilizer generators: ~a\n" (length generators))
  ; (printf "Size of stabilizer group: ~a\n" (expt 2 (length generators)))
  ; (printf "Number of error generators: ~a\n" (length error-generators))
  ; (printf "Number of equivalence classes of logical errors: ~a\n" (- (expt 2 (length error-generators)) 1))
  ; (printf "Number of logical errors: ~a\n" (* (expt 2 (length generators)) (- (expt 2 (length error-generators)) 1)))

;   (for ([i (in-range 100)])
;       (println (pauli->symbols (sample-logical-error generators error-generators)))
;   )

  )
