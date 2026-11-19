#lang racket/base

(require "src/paulis.rkt"
         "src/stabilizer-codes.rkt"
         "src/circuits.rkt")

(provide (all-from-out "src/paulis.rkt"
                       "src/stabilizer-codes.rkt"
                       "src/circuits.rkt"))

(module+ main
  (require "src/demo.rkt")
  (displayln (run-demo)))
