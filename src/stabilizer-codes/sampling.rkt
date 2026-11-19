#lang rosette

(require "../paulis/definitions.rkt"
         "../paulis/algebra.rkt"
         "../paulis/sampling.rkt"
         "logical.rkt")

(provide (all-defined-out))

(define (sample-logical-error generators error-generators)
  (define errs (list->vector error-generators))
  (let ([canonical-err (sample-group error-generators #:allow-empty #f)]
        [P (sample-group generators)])
    (pauli-mult P canonical-err)))

(define (sample-logical-error* generators)
  (sample-logical-error generators
                        (find-logical-error-generators generators)))

(define (probability-weight-given-logical-error k generators num-shots)
  (define error-generators (find-logical-error-generators generators))
  (define count 0)
  (for ([_ (in-range num-shots)])
    (define P (sample-logical-error generators error-generators))
    (cond
      [(<= (pauli-weight P) k)
       (set! count (add1 count))]))
  (exact->inexact (/ count num-shots)))

