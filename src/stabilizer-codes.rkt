#lang racket/base

(require "stabilizer-codes/logical.rkt"
         "stabilizer-codes/sampling.rkt")

(provide syndrome-of
         logical-error?
         find-logical-error
         find-logical-error-generators
         define-symbolic-syndrome
         sample-logical-error
         sample-logical-error*
         probability-weight-given-logical-error
         
         ; decoders
         decoder-logical-error?
         find-decoder-logical-error
         find-decoder-logical-error-generators
)
