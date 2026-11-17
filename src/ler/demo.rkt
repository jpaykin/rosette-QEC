#lang rosette

(require "pauli.rkt"
         "algebra.rkt"
         "logical.rkt"
         "sampling.rkt"
         "examples/toric-code.rkt")

(provide run-demo)

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

  ; (pauli->symbols (find-decoder-logical-error repetition-code-decoder repetition-code '()))



  ; (for ([P (find-decoder-logical-error-generators repetition-code-decoder repetition-code)])
  ;   (displayln (pauli->symbols P))
  ; )


  (define generators (toric-code 4))
  ;(find-logical-error generators)
  (define error-generators (find-logical-error-generators generators))
  (print-paulis error-generators)


  (printf "Number of stabilizer generators: ~a\n" (length generators))
  (printf "Size of stabilizer group: ~a\n" (expt 2 (length generators)))
  (printf "Number of error generators: ~a\n" (length error-generators))
  (printf "Number of equivalence classes of logical errors: ~a\n" (- (expt 2 (length error-generators)) 1))
  (printf "Number of logical errors: ~a\n" (* (expt 2 (length generators)) (- (expt 2 (length error-generators)) 1)))

;   (for ([i (in-range 100)])
;       (println (pauli->symbols (sample-logical-error generators error-generators)))
;   )

  )
