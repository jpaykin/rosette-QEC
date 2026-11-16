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

(define (sample-group generators #:allow-empty [allow-empty #t])
  (unless (wf-generators? generators)
    (raise-argument-error
      'sample-group
      "wf-generators?"
      generators)
  )

  (define width (pauli-width (first generators)))
  (define P (pauli-id width))
  (define empty? #t)

  (for ([G (in-list generators)])
    (cond
      [(sample-bool) 
       (set! P (pauli-mult P G))
       (set! empty? #f)
       ]))
  (if (and empty (not allow-empty))
    ( ; Pick an arbitrary generator of the group
      list-ref generators (random (length generators) RAND)
    )
    ; else
    P))

(define (sample-logical-error generators error-generators)
  ; TODO: check that generators is non-empty
  ; TODO: check that error-generators is non-empty
  (define errs (list->vector error-generators))
  (let ([canonical-err (sample-group generators #:allow-empty #f)]
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

