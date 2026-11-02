#lang rosette

(require "../pauli.rkt"
         "../algebra.rkt")

(provide (struct-out graph)
         edge-index
         edge->pauli
         vertex-stabilizer
         vertex-stabilizers
         get-faces
         face-stabilizer
         faces-stabilizers
         stabilizers
         valid-edge?
         wf-edges?
         wf-graph?
         edges->graph
         torus-graph
         toric-code)

;; The toric code starts as a graph
;;      num-vertices : int
;;      edges : list (int * int)
;;      adjacencies : list  (list int)
;; Here, edges is a list of edges. Edges are unordered
;; and sparse, so they are represented as (v1,v2) where v1 < v2.
;; Each edge (v1,v2) is associated with its index in the edges list.
;;
;; adjacencies[v] lists the neighboring vertices of v.
(struct graph (num-vertices edges adjacencies) #:transparent)
(struct torus-grid-graph graph (rows columns) #:transparent)

;; The index of an edge is its position in the edges list
(define (edge-index g v1 v2)
  (define idx
    (for/first ([edge (in-list (graph-edges g))]
                [i (in-naturals)]
                #:when (or (equal? edge (list v1 v2))
                           (equal? edge (list v2 v1))))
      i))
  (unless idx
    (error 'edge-index "edge not found: ~a -- ~a" v1 v2))
  idx)

;; Given a graph g with an edge (v1,v2)
;; and a single-qubit pauli p1,
;; return the Pauli p1_{(v1,v2)} associated
;; with the edge
(define (edge->pauli g p1 v1 v2)
  (define n (length (graph-edges g)))
  (define idx (edge-index g v1 v2))
  (bits->pauli n idx
               (pauli1-x p1)
               (pauli1-z p1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining the stabilizers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;      |X
;;      |
;; -X-- v ---X-
;;      |
;;      |X
(define (vertex-stabilizer g v)
    ;; We expect g to have 4 edges connected to v,
    ;; but this doesn't check that
    (define (f neighbor P)
        (pauli-mult P (edge->pauli g pauli1X v neighbor))
    )
    (foldr f (pauli-id (length (graph-edges g)))
           (list-ref (graph-adjacencies g) v))
)
(define (vertex-stabilizers g)
  (for/list ([v (in-range (graph-num-vertices g))])
    (vertex-stabilizer g v)))

;; v1 ----Z----- v2
;;  |            |
;; Z|            |Z
;;  |            |
;; v3 ----Z----- v4
;;
;; Return a list of faces (4-tuples of vertices)
(define (get-faces g)
  (if (torus-grid-graph? g)
      (let ([m (torus-grid-graph-rows g)]
            [n (torus-grid-graph-columns g)])
        (define (vertex row column)
          (+ (* row n) column))
        (for*/list ([row (in-range m)]
                    [column (in-range n)])
          (list (vertex row column)
                (vertex row (modulo (add1 column) n))
                (vertex (modulo (add1 row) m)
                        (modulo (add1 column) n))
                (vertex (modulo (add1 row) m) column))))
      (let ()
        (define (neighbors v)
          (append
           (for/list ([edge (in-list (graph-edges g))]
                      #:when (equal? (first edge) v))
             (second edge))
           (for/list ([edge (in-list (graph-edges g))]
                      #:when (equal? (second edge) v))
             (first edge))))
        (for*/list ([v1 (in-range (graph-num-vertices g))]
                    [v2 (in-list (neighbors v1))]
                    [v3 (in-list (neighbors v2))]
                    [v4 (in-list (neighbors v3))]
                    #:when (and (< v1 v2)
                                (< v1 v3)
                                (< v1 v4)
                                (< v2 v4)
                                (member v1 (neighbors v4))
                                (not (= v2 v3))
                                (not (= v2 v4))
                                (not (= v3 v4))))
          (list v1 v2 v3 v4)))))

(define (face-stabilizer g v1 v2 v3 v4)
    (pauli-mult 
        (edge->pauli g pauli1Z v1 v2)
        (edge->pauli g pauli1Z v2 v3)
        (edge->pauli g pauli1Z v3 v4)
        (edge->pauli g pauli1Z v4 v1)
    ))

(define (faces-stabilizers g)
  (for/list ([face (in-list (get-faces g))])
    (apply face-stabilizer g face)))

(define (stabilizers g)
    (append (vertex-stabilizers g) (faces-stabilizers g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining the graph structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valid-edge? num-vertices edge)
    (and (list? edge)
         (= (length edge) 2)
         (andmap exact-nonnegative-integer? edge)
         (< (first edge) num-vertices)
         (< (second edge) num-vertices)
         (not (= (first edge) (second edge)))))

(define (wf-edges? num-vertices edges)
  (and (exact-nonnegative-integer? num-vertices)
       (list? edges)
       (andmap (lambda (edge) (valid-edge? num-vertices edge)) edges)
       (not
        (check-duplicates
         (map (lambda (edge)
                (list (min (first edge) (second edge))
                      (max (first edge) (second edge))))
              edges)
         equal?))))

(define (wf-graph? g)
  (and (graph? g)
       (wf-edges? (graph-num-vertices g) (graph-edges g))
       (equal? (graph-edges g)
               (map (lambda (edge)
                      (list (min (first edge) (second edge))
                            (max (first edge) (second edge))))
                    (graph-edges g)))
       (equal? (graph-adjacencies g)
               (graph-adjacencies
                (edges->graph (graph-num-vertices g)
                              (graph-edges g))))))

(define (edges->graph num-vertices edges)
  (unless (wf-edges? num-vertices edges)
    (raise-arguments-error
     'edges->graph
     "expected well-formed edges for the given vertex count"
     "num-vertices" num-vertices
     "edges" edges))
  (define normalized-edges
    (for/list ([edge (in-list edges)])
      (define v1 (first edge))
      (define v2 (second edge))
      (list (min v1 v2) (max v1 v2))))
  (define adjacency-vectors
    (build-vector num-vertices (lambda (_) '())))
  (for ([edge (in-list normalized-edges)])
    (define v1 (first edge))
    (define v2 (second edge))
    (vector-set! adjacency-vectors v1
                 (cons v2 (vector-ref adjacency-vectors v1)))
    (vector-set! adjacency-vectors v2
                 (cons v1 (vector-ref adjacency-vectors v2))))
  (graph num-vertices
         normalized-edges
         (for/list ([neighbors (in-vector adjacency-vectors)])
           (reverse neighbors))))

(define (torus-graph m n)
  (unless (and (exact-integer? m) (>= m 3))
    (raise-argument-error 'torus-graph "exact integer >= 3" m))
  (unless (and (exact-integer? n) (>= n 3))
    (raise-argument-error 'torus-graph "exact integer >= 3" n))
  (define (vertex row column)
    (+ (* row n) column))
  (define edges
    (append
     (for*/list ([row (in-range m)]
                 [column (in-range n)])
       (list (vertex row column)
             (vertex row (modulo (add1 column) n))))
     (for*/list ([row (in-range m)]
                 [column (in-range n)])
       (list (vertex row column)
             (vertex (modulo (add1 row) m) column)))))
  (define g (edges->graph (* m n) edges))
  (torus-grid-graph (graph-num-vertices g)
                    (graph-edges g)
                    (graph-adjacencies g)
                    m
                    n))

(define (toric-code l)
    (stabilizers (torus-graph l l)))