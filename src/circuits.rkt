#lang racket/base

(require "circuits/circuit.rkt")
(require "circuits/synthesis.rkt")

(provide (struct-out circuit)
         (struct-out gate-h)
         (struct-out gate-s)
         (struct-out gate-cx)
         (struct-out gate-pauli)
         (struct-out gate-fault)
         (struct-out measure-qubit)
         (struct-out reset-qubit)
         (struct-out detector)
         (struct-out observable)
         (struct-out circuit-result)
         run-circuit
         undetected-logical-error?
         find-circuit-logical-error
         find-circuit-error-generators
         )
