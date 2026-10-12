#lang rosette

(require "pauli.rkt"
         "logical.rkt"
         "sampling.rkt")

(provide run-demo)

(define (run-demo)
  (displayln (pretty-pauli1 (pauli1 #f #t)))
  (probability-weight-given-logical-error 3 five-qubit-code 1000))
