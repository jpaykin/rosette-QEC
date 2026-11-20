#lang racket/base

(require "paulis/definitions.rkt"
         "paulis/algebra.rkt"
         "paulis/sampling.rkt"
         "paulis/synthesis.rkt")

(provide (struct-out pauli1)
         (struct-out pauli)
         
         ;; constructors
         define-symbolic-pauli
         define-symbolic-pauli1
         pauli-id
         pauli1X
         pauli1Y
         pauli1Z
         pauli1I
         symbol->pauli
         bits->pauli
         symbol->pauli1
         symbols->pauli

         ;; well-formedness
         single-qubit-pauli?
         wf-pauli?
         wf-generators?

         ;; operators
         pauli1-at
         pauli-weight
         commute?
         pauli-mult

         ;; pretty-printers
         pauli1->symbol
         pauli->symbols
         pretty-pauli1
         pretty-pauli
         print-paulis

         ;; group operators
         pauli-in-group?
         reduce-generators
         sample-group

         ;; Pauli synthesis that satisfies a given property
         synthesize-pauli
         ;; Synthesize a set of linearly independent Paulis
         ;; that satisfy a given property
         synthesize-generators
         )
