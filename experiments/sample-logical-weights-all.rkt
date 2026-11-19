 
#lang rosette

;; Sample uniformly from the 15 nontrivial logical Pauli classes
;; of an L x L toric code.
;; Paulis are represented modulo global phase.

(require "../src/paulis.rkt"
         "../src/stabilizer-codes.rkt"
         (only-in "../src/paulis/sampling.rkt" sample-bool)
         "../src/examples/toric-code.rkt"
         racket/file)

(provide run-logical-weight-experiment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Toric lattice geometry ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We want this toric lattice geometry to be consistent with the toric-code.rkt implementation. This is used to construct the logical representatives of the toric code, which are noncontractible loops on the torus. 

;; Vertices have row-major indices.
(define (torus-vertex L row col)
  (+ (* row L) col))

;; Horizontal edge from (row, col) to (row, col + 1).
(define (horizontal-edge L row col)
  (list (torus-vertex L row col)
        (torus-vertex L row (modulo (add1 col) L))))

;; Vertical edge from (row, col) to (row + 1, col).
(define (vertical-edge L row col)
  (list (torus-vertex L row col)
        (torus-vertex L (modulo (add1 row) L) col)))

;; Construct a physical Pauli operator on a sequence of edges.
;; The repository's edge->pauli determines the physical qubit index.

;;loop-operator takes a graph g, a kind of Pauli (X or Z), and a list of edges, and returns the product of the corresponding physical Paulis on those edges.

;; this is used to construct the logical operators of the toric code, which are noncontractible loops on the torus.

(define (loop-operator g kind edges)
  (define n (length (graph-edges g)))
  (for/fold ([P (pauli-id n)])
            ([edge (in-list edges)])
    (pauli-mult P
                (edge->pauli g kind
                             (first edge)
                             (second edge)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Construct logical classes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The toric code has two logical qubits, so there are 15 nontrivial logical Pauli classes.

;;toric-logical-classes takes a graph g and an integer L, and returns a list of 15 pairs, where each pair consists of a string label (like "X1Z2") and a corresponding Pauli operator that represents a nontrivial logical class.

(define (toric-logical-classes g L)
  (define n (length (graph-edges g)))

  ;; Z1: horizontal noncontractible loop.
  (define Z1
    (loop-operator
     g pauli1Z
     (for/list ([c (in-range L)])
       (horizontal-edge L 0 c))))

  ;; X1: vertical loop, supported on horizontal edges.
  (define X1
    (loop-operator
     g pauli1X
     (for/list ([r (in-range L)])
       (horizontal-edge L r 0))))

  ;; Z2:vertical noncontractible loop.
  (define Z2
    (loop-operator
     g pauli1Z
     (for/list ([r (in-range L)])
       (vertical-edge L r 0))))

  ;; X2: dual horizontal loop, supported on vertical edges.
  (define X2
    (loop-operator
     g pauli1X
     (for/list ([c (in-range L)])
       (vertical-edge L 0 c))))

  ;; Verify that all four logical operators commute
  ;; with every stabilizer.
  (define basis (list X1 Z1 X2 Z2))
  (define G (stabilizers g))

  (unless
      (for*/and ([P (in-list basis)]
                 [S (in-list G)])
        (commute? P S))
    (error 'toric-logical-classes
           "logical loop does not commute with stabilizers"))

  ;; Verify the logical Pauli commutation relations:
  ;; X1 anticommutes with Z1,
  ;; X2 anticommutes with Z2,
  ;; and all operators on different logical qubits commute.
  (unless
      (and (not (commute? X1 Z1))
           (not (commute? X2 Z2))
           (commute? X1 X2)
           (commute? X1 Z2)
           (commute? Z1 X2)
           (commute? Z1 Z2))
    (error 'toric-logical-classes
           "logical operators do not form two Pauli pairs"))

  ;; Each basic noncontractible loop should have weight L.
  (unless
      (andmap (lambda (P) (= (pauli-weight P) L))
              basis)
    (error 'toric-logical-classes
           "expected logical loop weight L"))

  ;; Construct I, X, Y, or Z on one logical qubit.
  ;; Y = XZ modulo global phase.
  (define (one-logical op X Z)
    (case op
      [(I) (pauli-id n)]
      [(X) X]
      [(Y) (pauli-mult X Z)]
      [(Z) Z]))

  ;; Generate every two-qubit logical Pauli product except II.
  ;; This produces 15 distinct nontrivial logical classes.
  (for*/list ([a (in-list '(I X Y Z))]
              [b (in-list '(I X Y Z))]
              #:unless (and (eq? a 'I)
                            (eq? b 'I)))
    (cons
     (string-append (symbol->string a)
                    (symbol->string b))
     (pauli-mult
      (one-logical a X1 Z1)
      (one-logical b X2 Z2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Sample a stabilizer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Randomly select a subset of the stabilizer generators.
;; Each generator is included independently with probability 1/2.
;;
;; This samples uniformly from the stabilizer group,
;; even when the generator list is redundant.
;;
;; The generators are validated before the sampling loop.

;; sample-stabilizer takes a list of stabilizer generators and the number of physical qubits n, and returns a random stabilizer operator sampled uniformly from the group generated by those generators.
(define (sample-stabilizer generators n)
  (for/fold ([S (pauli-id n)])
            ([G (in-list generators)])
    (if (sample-bool)
        (pauli-mult S G)
        S)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Run the experiment    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run-logical-weight-experiment takes an integer L and an integer shots, and runs the logical weight sampling experiment for the toric code of size L, taking shots samples. It saves the results to a CSV file and prints a summary of the observed weights.

(define (run-logical-weight-experiment L shots)

  ;; Validate command-line parameters.
  (unless (and (exact-integer? L) (>= L 3))
    (raise-argument-error
     'run-logical-weight-experiment
     "exact integer >= 3"
     L))

  (unless (and (exact-integer? shots) (positive? shots))
    (raise-argument-error
     'run-logical-weight-experiment
     "positive exact integer"
     shots))


  ;; Construct the toric lattice and its stabilizers.
  (define g (torus-graph L L))
  (define G (stabilizers g))
  (define n (length (graph-edges g)))

  (unless (= n (* 2 L L))
    (error 'run-logical-weight-experiment
           "unexpected number of physical qubits"))

  ;; Validate stabilizers once, outside the sampling loop.
  (unless
      (and (andmap wf-pauli? G)
           (for/and ([P (in-list G)])
             (= (pauli-width P) n))
           (for*/and ([i (in-range (length G))]
                      [j (in-range (add1 i) (length G))])
             (commute? (list-ref G i)
                       (list-ref G j))))
    (error 'run-logical-weight-experiment
           "invalid toric stabilizers"))

  ;; Construct all 15 logical class representatives.
  (define classes (toric-logical-classes g L))

  (unless (= (length classes) 15)
    (error 'run-logical-weight-experiment
           "expected 15 nontrivial logical classes"))

  (printf "Toric code L=~a: n=~a physical qubits, d=~a\n"
          L n L)

  (printf "Sampling ~a errors across ~a logical classes\n"
          shots (length classes))

  (printf "Class labels: ~a\n" (map car classes))

  ;; Each sample:
  ;; 1. Select one of 15 logical representatives uniformly.
  ;; 2. Sample a random stabilizer S.
  ;; 3. Compute P = S * L_i.
  ;; 4. Record the class and physical Pauli weight.
  (define samples
    (for/list ([i (in-range 1 (add1 shots))])
      (define selected
        (list-ref classes (random (length classes))))

      (define S (sample-stabilizer G n))
      (define P (pauli-mult S (cdr selected)))
      (define w (pauli-weight P))

      (unless (and (exact-integer? w)
                   (<= L w n))
        (error 'run-logical-weight-experiment
               "invalid weight ~a for class ~a"
               w (car selected)))

      (list i (car selected) w)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 5. Save the experiment  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (make-directory* "results")

  (define output-file
    (format "results/toric-L~a-all15-N~a.csv"
            L shots))

  (call-with-output-file output-file
    (lambda (out)
      (displayln "sample,class,weight" out)
      (for ([row (in-list samples)])
        (fprintf out "~a,~a,~a\n"
                 (first row)
                 (second row)
                 (third row))))
    #:exists 'replace)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 6. Print summary        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define weights (map third samples))

  (printf "Minimum observed weight: ~a\n"
          (apply min weights))

  (printf "Maximum observed weight: ~a\n"
          (apply max weights))

  (printf "Mean observed weight: ~a\n"
          (exact->inexact (/ (apply + weights) shots)))

  (printf "Expected mean weight: ~a\n"
          (* 3/4 n))

  (printf "Saved: ~a\n" output-file)

  output-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Command-line entry    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (define args
    (vector->list (current-command-line-arguments)))

  (unless (or (null? args) (= (length args) 2))
    (error 'sample-logical-weights-all
           "usage: racket experiments/sample-logical-weights-all.rkt [L SHOTS]"))

  (define L
    (if (null? args)
        3
        (string->number (first args))))

  (define shots
    (if (null? args)
        1000
        (string->number (second args))))

  (run-logical-weight-experiment L shots))
