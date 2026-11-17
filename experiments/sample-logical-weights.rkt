#lang rosette

;; First toric-code sampling experiment: sample from the logical-error
;; representatives produced by the existing implementation in logical.rkt.

(require "../src/ler/pauli.rkt"
         "../src/ler/algebra.rkt"
         "../src/ler/logical.rkt"
         "../src/ler/sampling.rkt"
         "../src/ler/examples/toric-code.rkt"
         racket/file)

(provide run-logical-weight-experiment)

;; parameter checks: is L a positive integer >= 3? is shots a positive integer?
(define (run-logical-weight-experiment L shots)
  (unless (and (exact-integer? L) (>= L 3))
    (raise-argument-error 'run-logical-weight-experiment "exact integer >= 3" L))
  (unless (and (exact-integer? shots) (positive? shots))
    (raise-argument-error 'run-logical-weight-experiment "positive exact integer" shots))

;; define n as the number of physical qubits in the toric code, which is 2 * L * L
;; define G as the toric code graph for the given L

  (define n (* 2 L L))
  (define G (toric-code L))

  ;;run the solver only once, not once per sample.
  ;; representatives is a list of two nontrivial logical representatives for the toric code.
  (define representatives (find-logical-error-generators G))
  
  ;; why four? because the toric code has two logical qubits
  (unless (= (length representatives) 4)
    (error 'run-logical-weight-experiment
           "expected 4 logical representatives for the toric code, got ~a"
           (length representatives)))

  ;; Check that all representatives really are nontrivial logical errors.
  (for ([P (in-list representatives)] [i (in-naturals 1)])
    (unless (logical-error? P G)
      (error 'run-logical-weight-experiment
             "representative ~a is not a nontrivial logical error" i)))

  (printf "Toric code: L=~a, physical qubits=~a\n" L n)
  (printf "Sampling ~a errors using ~a logical representatives:\n"
          shots (length representatives))
  (for ([P (in-list representatives)] [i (in-naturals 1)])
    (printf "  Representative ~a: ~a (weight ~a)\n"
            i (pretty-pauli P) (pauli-weight P)))

  ;; Each sample is S * P_i, where S is a random stabilizer and
  ;; P_i is one of the logical representatives selected above.
  (define weights
    (for/list ([j (in-range shots)])
      (define P (sample-logical-error G representatives))

      ; For debugging:
      ; (cond
      ;   [(= (modulo j 100) 0)
      ;    (printf "Sample ~a: ~a\n" j (pretty-pauli P))])

      (pauli-weight P)))

  (make-directory* "results")
  (define output-file
    (format "results/toric-L~a-generic.csv" L))
  (call-with-output-file output-file
    (lambda (out)
      (displayln "sample,weight" out)
      (for ([w (in-list weights)] [i (in-naturals 1)])
        (fprintf out "~a,~a\n" i w)))
    #:exists 'replace)

  (printf "Minimum weight: ~a\n" (apply min weights))
  (printf "Maximum weight: ~a\n" (apply max weights))
  (printf "Mean weight: ~a\n" (exact->inexact (/ (apply + weights) shots)))
  (printf "Saved: ~a\n" output-file)
  output-file)

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (unless (or (null? args) (= (length args) 2))
    (error 'sample-logical-weights
           "usage: racket experiments/sample-logical-weights.rkt [L SHOTS]"))
  (define L (if (null? args) 3 (string->number (first args))))
  (define shots (if (null? args) 1000 (string->number (second args))))
  (run-logical-weight-experiment L shots))
