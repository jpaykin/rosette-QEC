#lang rosette

(require "pauli.rkt"
         "algebra.rkt"
         "logical.rkt")

(provide sample-bool
         sample-group
         sample-logical-error
         sample-logical-error*
         probability-weight-given-logical-error)

(define RAND (make-pseudo-random-generator))

(define (sample-bool)
  (eq? 1 (random 2 RAND)))

(define (sample-group generators)
  (assert (wf-generators? generators))

  (define width (pauli-width (first generators)))
  (define P (pauli-id width))

  (for ([G (in-list generators)])
    (cond
      [(sample-bool) (set! P (pauli-mult P G))]))
  P)

(define (sample-logical-error generators error-generators)
  (define errs (list->vector error-generators))
  (let ([i (random (length error-generators) RAND)]
        [P (sample-group generators)])
    (pauli-mult P (vector-ref errs i))))

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
