#lang rosette

(require "pauli.rkt"
         "algebra.rkt")

(provide logical-error?
         find-logical-error
         find-logical-error-generators
         five-qubit-code
         tiny-example-code)

(define (logical-error? P generators)
  (&& (not (pauli-in-group? P generators))
      (andmap (lambda (Q) (commute? P Q)) generators)))

(define (find-logical-error generators)
  (assert (wf-generators? generators))

  (println "Trying to find a logical error given generators:")
  (print-paulis generators)

  (define width (pauli-width (first generators)))
  (define-symbolic-pauli symbolic-p width)
  (define sol (solve (assert (logical-error? symbolic-p generators))))
  (if (unsat? sol)
      (error "Could not find a logical error")
      (begin
        (define concrete-p (evaluate symbolic-p sol))
        (println (pretty-pauli concrete-p))
        concrete-p)))

(define five-qubit-code
  (list (symbols->pauli '(X Z Z X I))
        (symbols->pauli '(I X Z Z X))
        (symbols->pauli '(X I X Z Z))
        (symbols->pauli '(Z X I X Z))))

(define tiny-example-code
  (list (symbols->pauli '(X Z Z X I))))

(define (find-logical-error-generators generators)
  (assert (wf-generators? generators))

  (define width (pauli-width (first generators)))
  (define num-log-errors (- width (length generators)))

  (define errors (make-vector num-log-errors (pauli-id width)))
  (for ([i (in-range num-log-errors)])
    (define new-generators (append generators (vector->list errors)))
    (define P (find-logical-error new-generators))
    (vector-set! errors i P))
  (vector->list errors))
