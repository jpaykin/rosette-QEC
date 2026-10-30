#lang racket/base

(require rackunit
         rosette
         "../src/ler/pauli.rkt"
         "../src/ler/algebra.rkt"
         "../src/ler/examples/toric-code.rkt")

(define square
  (edges->graph 4 '((0 1) (1 2) (2 3) (3 0))))

(define torus-3x3 (torus-graph 3 3))

(test-case "valid-edge? checks endpoint shape and range"
  (check-true  (valid-edge? 4 '(0 3)))
  (check-false (valid-edge? 4 '(1 1)))
  (check-false (valid-edge? 4 '(0 4)))
  (check-false (valid-edge? 4 '(0 -1)))
  (check-false (valid-edge? 4 '(0)))
  (check-false (valid-edge? 4 '(0 1 2)))
  (check-false (valid-edge? 4 '(0 x))))

(test-case "wf-edges? accepts valid undirected edge lists"
  (check-true  (wf-edges? 4 '()))
  (check-true  (wf-edges? 4 '((0 1) (1 2) (2 3))))
  (check-false (wf-edges? -1 '()))
  (check-false (wf-edges? 4 'not-an-edge-list))
  (check-false (wf-edges? 4 '((0 4))))
  (check-false (wf-edges? 4 '((2 2))))
  (check-false (wf-edges? 4 '((0 1) (1 0)))))

(test-case "edges->graph normalizes edges and builds adjacency lists"
  (define g (edges->graph 5 '((2 1) (3 2) (4 0))))
  (check-equal? (graph-num-vertices g) 5)
  (check-equal? (graph-edges g) '((1 2) (2 3) (0 4)))
  (check-equal? (graph-adjacencies g) '((4) (2) (1 3) (2) (0)))
  (check-true (wf-graph? g))
  (check-exn exn:fail?
             (lambda () (edges->graph 2 '((0 2)))))
  (check-exn exn:fail?
             (lambda () (edges->graph 2 '((0 1) (1 0)))))
  (check-exn exn:fail?
             (lambda () (edges->graph -1 '()))))

(test-case "empty and isolated-vertex graphs are well formed"
  (define empty-graph (edges->graph 0 '()))
  (define isolated-graph (edges->graph 4 '()))
  (check-equal? (graph-num-vertices empty-graph) 0)
  (check-equal? (graph-adjacencies empty-graph) '())
  (check-true (wf-graph? empty-graph))
  (check-equal? (graph-edges isolated-graph) '())
  (check-equal? (graph-adjacencies isolated-graph) '(() () () ()))
  (check-true (wf-graph? isolated-graph)))

(test-case "wf-graph? rejects inconsistent graph fields"
  (check-true (wf-graph? square))
  (check-false (wf-graph? 'not-a-graph))
  (check-false
   (wf-graph? (graph 4 '((0 1) (1 2) (2 3) (0 3))
                     '(() (0 2) (1 3) (2 0)))))
  (check-false
   (wf-graph? (graph 4 '((1 0) (1 2) (2 3) (0 3))
                     '((1 3) (0 2) (1 3) (2 0)))))
  (check-false
   (wf-graph? (graph 4 '((0 1) (1 2) (2 3) (0 3)) 'not-an-adjacency-list)))
  (check-false
   (wf-graph? (graph 4 '((0 1) (1 2) (2 3) (0 3))
                     '((1 3) (0 2) (1 3))))))

(test-case "edge-index is stable and treats edges as undirected"
  (check-equal? (edge-index square 0 1) 0)
  (check-equal? (edge-index square 1 0) 0)
  (check-equal? (edge-index square 2 3) 2)
  (check-exn exn:fail? (lambda () (edge-index square 0 2)))
  (check-exn exn:fail? (lambda () (edge-index square -1 0)))
  (check-exn exn:fail? (lambda () (edge-index square 0 4))))

(test-case "edge->pauli places a single-qubit Pauli at the edge index"
  (check-equal? (pauli->symbols (edge->pauli square pauli1X 1 2))
                '(I X I I))
  (check-equal? (pauli->symbols (edge->pauli square pauli1Z 0 3))
                '(I I I Z))
  (check-equal? (pauli->symbols (edge->pauli square pauli1Y 3 2))
                '(I I Y I))
  (check-exn exn:fail? (lambda () (edge->pauli square pauli1X 0 2))))

(test-case "torus-graph builds a wrapped simple grid"
  (check-equal? (graph-num-vertices torus-3x3) 9)
  (check-equal? (length (graph-edges torus-3x3)) 18)
  (check-true (wf-graph? torus-3x3))
  (check-equal? (graph-edges torus-3x3)
                '((0 1) (1 2) (0 2)
                  (3 4) (4 5) (3 5)
                  (6 7) (7 8) (6 8)
                  (0 3) (1 4) (2 5)
                  (3 6) (4 7) (5 8)
                  (0 6) (1 7) (2 8)))
  (check-true
   (andmap (lambda (neighbors) (= (length neighbors) 4))
           (graph-adjacencies torus-3x3)))
  (check-exn exn:fail:contract? (lambda () (torus-graph 2 3)))
  (check-exn exn:fail:contract? (lambda () (torus-graph 3 2)))
  (check-exn exn:fail:contract? (lambda () (torus-graph 3.5 4)))
  (check-exn exn:fail:contract? (lambda () (torus-graph 0 3)))
  (check-exn exn:fail:contract? (lambda () (torus-graph -3 4)))
  (check-exn exn:fail:contract? (lambda () (torus-graph 'three 4)))
  (check-exn exn:fail:contract? (lambda () (torus-graph 3 'four)))
  (check-exn exn:fail:contract? (lambda () (toric-code 2)))
  (check-exn exn:fail:contract? (lambda () (toric-code 0)))
  (check-exn exn:fail:contract? (lambda () (toric-code -3))))

(test-case "rectangular tori have expected graph and stabilizer counts"
  (for ([dimensions (in-list '((3 4) (4 5) (5 3)))])
    (define m (first dimensions))
    (define n (second dimensions))
    (define g (torus-graph m n))
    (define num-vertices (* m n))
    (define num-edges (* 2 num-vertices))
    (check-equal? (graph-num-vertices g) num-vertices)
    (check-equal? (length (graph-edges g)) num-edges)
    (check-true (wf-graph? g))
    (check-true
     (andmap (lambda (neighbors) (= (length neighbors) 4))
             (graph-adjacencies g)))
    (define faces (get-faces g))
    (check-equal? (length faces) num-vertices)
    (check-equal? (length (remove-duplicates faces)) num-vertices)
    (for ([face (in-list faces)])
      (for ([i (in-range 4)])
        (check-not-false
         (member (list-ref face (modulo (add1 i) 4))
                 (list-ref (graph-adjacencies g)
                           (list-ref face i))))))
    (check-equal? (length (stabilizers g)) (* 2 num-vertices))
    (define generators (stabilizers g))
    (for ([generator (in-list generators)])
      (check-equal? (pauli-width generator) num-edges)
      (check-equal? (count (lambda (symbol) (not (eq? symbol 'I)))
                           (pauli->symbols generator))
                    4))
    (for* ([i (in-range (length generators))]
           [j (in-range (add1 i) (length generators))])
      (check-true (commute? (list-ref generators i)
                            (list-ref generators j))))))

(test-case "get-faces returns each elementary torus face once"
  (define faces (get-faces torus-3x3))
  (check-equal? (length faces) 9)
  (check-equal? (length (remove-duplicates faces)) 9)
  (for ([face (in-list faces)])
    (check-equal? (length face) 4)
    (for ([i (in-range 4)])
      (check-not-false
       (member (list-ref face i)
               (list-ref (graph-adjacencies torus-3x3)
                         (list-ref face (modulo (add1 i) 4)))))))
  (check-equal? (get-faces square) '((0 1 2 3))))

(test-case "plaquette and face stabilizers have edge width and weight four"
  (define plaquette (plaquette-stabilizer torus-3x3 0))
  (define face (face-stabilizer torus-3x3 0 1 4 3))
  (check-equal? (pauli-width plaquette) 18)
  (check-equal? (pauli-width face) 18)
  (check-equal? (pauli->symbols plaquette)
                '(X I X I I I I I I X I I I I I X I I))
  (check-equal? (pauli->symbols face)
                '(Z I I Z I I I I I Z Z I I I I I I I))
  (check-equal? (length (plaquette-stabilizers torus-3x3)) 9)
  (check-equal? (length (faces-stabilizers torus-3x3)) 9)
  (check-equal? (length (stabilizers torus-3x3)) 18)
  (for ([stabilizer (in-list (stabilizers torus-3x3))])
    (check-equal? (pauli-width stabilizer) 18)
    (check-equal? (count (lambda (symbol) (not (eq? symbol 'I)))
                         (pauli->symbols stabilizer))
                  4)))

(test-case "toric-code returns commuting stabilizers"
  (define generators (toric-code 3))
  (check-equal? (length generators) 18)
  (for* ([i (in-range (length generators))]
         [j (in-range (add1 i) (length generators))])
    (check-true (commute? (list-ref generators i)
                          (list-ref generators j)))))

(test-case "larger square toric code has expected generator width and count"
  (define generators (toric-code 4))
  (check-equal? (length generators) 32)
  (for ([generator (in-list generators)])
    (check-equal? (pauli-width generator) 32))
  (for* ([i (in-range (length generators))]
         [j (in-range (add1 i) (length generators))])
    (check-true (commute? (list-ref generators i)
                          (list-ref generators j)))))
