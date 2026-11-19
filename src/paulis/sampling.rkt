#lang rosette

(require "definitions.rkt"
         "algebra.rkt")

(provide (all-defined-out))

(define RAND (make-pseudo-random-generator))

(define (sample-bool)
  (eq? 1 (random 2 RAND)))

(define (sample-group generators #:allow-empty [allow-empty #t])
  (define width (pauli-width (first generators)))
  (define P (pauli-id width))
  (define empty? #t)

  (for ([G (in-list generators)])
    (cond
      [(sample-bool)
       (set! P (pauli-mult P G))
       (set! empty? #f)
       ]))
  (if (and empty? (not allow-empty))
    ( ; Pick an arbitrary generator of the group
      list-ref generators (random (length generators) RAND)
    )
    ; else
    P))
