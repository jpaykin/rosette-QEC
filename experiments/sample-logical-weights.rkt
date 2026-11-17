#lang rosette

;; Generic sampling experiment: given the stabilizer generators of any
;; stabilizer code, sample logical errors S * P_i, where S is a random
;; stabilizer and P_i is a logical-error representative found by the solver
;; in logical.rkt.

(require "../src/ler/pauli.rkt"
         "../src/ler/algebra.rkt"
         "../src/ler/logical.rkt"
         "../src/ler/sampling.rkt"
         "../src/ler/examples/toric-code.rkt"
         racket/file)

(provide run-logical-weight-experiment)

;; G: list of commuting stabilizer generators (same width).
;; shots: positive integer.
;; label: used in the output file name results/<label>-generic.csv.
(define (run-logical-weight-experiment G shots #:label [label "code"])
  (unless (wf-generators? G)
    (raise-argument-error 'run-logical-weight-experiment "wf-generators?" G))
  (unless (and (exact-integer? shots) (positive? shots))
    (raise-argument-error 'run-logical-weight-experiment "positive exact integer" shots))

  (define n (pauli-width (first G)))

  ;; Run the solver only once, not once per sample.
  (define representatives (find-logical-error-generators G))
  (when (null? representatives)
    (error 'run-logical-weight-experiment
           "code has no logical errors to sample"))

  ;; Check that all representatives really are nontrivial logical errors.
  (for ([P (in-list representatives)] [i (in-naturals 1)])
    (unless (logical-error? P G)
      (error 'run-logical-weight-experiment
             "representative ~a is not a nontrivial logical error" i)))

  (printf "Code ~a: physical qubits=~a, generators=~a\n" label n (length G))
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
    (format "results/~a-generic.csv" label))
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
  (unless (and (exact-integer? L) (>= L 3))
    (raise-argument-error 'sample-logical-weights "exact integer >= 3" L))
  (run-logical-weight-experiment (toric-code L) shots
                                 #:label (format "toric-L~a" L)))
