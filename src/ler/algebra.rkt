#lang rosette

;; Algebraic operations on Paulis

(require "pauli.rkt")

(provide commute?
         syndrome-of
         wf-generators?
         pauli-mult
         pauli-row-echelon
         print-paulis
         pauli-in-group?
         )

(define (bv-inner-product width u v)
  (for/fold ([parity (bv 0 (bitvector 1))])
            ([i (in-range width)])
    (bvxor parity
           (bvand (bit i u)
                  (bit i v)))))

(define (commute? P1 P2)
  (define w (pauli-width P1))
  ;;(assert (eq? (pauli-width P2) w))
  (bvzero?
   (bvsub (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
          (bv-inner-product w (pauli-zs P1) (pauli-xs P2)))))

(define (wf-generators? gens)
  (and
   (list? gens)
   (pair? gens)
   (let ([n (pauli-width (first gens))])
     (for ([i (in-range (length gens))])
       (let ([Pi (list-ref gens i)])
         (and
          (wf-pauli? Pi)
          (eq? (pauli-width Pi) n)))))
   (for* ([i (in-range (length gens))]
          [j (in-range (add1 i) (length gens))])
     (commute? (list-ref gens i) (list-ref gens j)))))

(define (pauli-column-bit P column)
  (define width (pauli-width P))
  (if (< column width)
      (bit column (pauli-xs P))
      (bit (- column width) (pauli-zs P))))

(define (pauli-mult . paulis)
  (unless (pair? paulis)
    (raise-argument-error 'pauli-mult "non-empty list of Paulis" paulis))
  (define first-pauli (car paulis))
  (for/fold ([product first-pauli])
            ([next-pauli (in-list (cdr paulis))])
    (pauli (pauli-width product)
           (bvxor (pauli-xs product) (pauli-xs next-pauli))
           (bvxor (pauli-zs product) (pauli-zs next-pauli)))))

(define (pauli-row-echelon generators width)
  (define rows (list->vector generators))
  (define pivot-count 0)
  (define pivot-columns '())
  (for ([column (in-range (* 2 width))]
        #:break (eq? pivot-count (vector-length rows)))
    (define pivot-index
      (for/first ([candidate-index
                   (in-range pivot-count (vector-length rows))]
                  #:when
                  (bitvector->bool
                   (pauli-column-bit (vector-ref rows candidate-index)
                                     column)))
        candidate-index))

    (when pivot-index
      (define pivot (vector-ref rows pivot-index))
      (define displaced (vector-ref rows pivot-count))
      (vector-set! rows pivot-count pivot)
      (vector-set! rows pivot-index displaced)
      (for ([row-index (in-range (vector-length rows))]
            #:unless (eq? row-index pivot-count))
        (define row (vector-ref rows row-index))
        (when (bitvector->bool (pauli-column-bit row column))
          (vector-set! rows row-index (pauli-mult row pivot))))
      (set! pivot-columns (cons column pivot-columns))
      (set! pivot-count (add1 pivot-count))))
  (values (for/list ([row-index (in-range pivot-count)])
            (vector-ref rows row-index))
          (reverse pivot-columns)))

(define (print-paulis generators)
  (for ([P (in-list generators)])
    (displayln (pretty-pauli P))))

;; syndrome of a Pauli P with respect to a list of generators of a Pauli group
(define (syndrome-of P G)
  (map (lambda (g) (not (commute? P g))) G)
)

(define (pauli-in-group? P generators)
  (unless (and (wf-pauli? P)
               (exact-positive-integer? (pauli-width P)))
    (raise-argument-error 'pauli-in-group? "wf-pauli? with positive width" P))
  (unless (and (list? generators) (pair? generators))
    (raise-argument-error 'pauli-in-group? "non-empty list?" generators))
  (for ([generator (in-list generators)])
    (unless (and (eq? (pauli-width generator) (pauli-width P))
                 (concrete? (pauli-xs generator))
                 (concrete? (pauli-zs generator)))
      (raise-argument-error
       'pauli-in-group?
       "list of concrete well-formed Paulis with matching width"
       generators)))
  (unless (wf-generators? generators)
    (raise-argument-error 'pauli-in-group? "wf-generators?" generators))

  (let-values ([(basis pivots)
                (pauli-row-echelon generators (pauli-width P))])
    (define residual
      (for/fold ([residual P])
                ([pivot (in-list pivots)]
                 [row (in-list basis)])
        (if (bitvector->bool (pauli-column-bit residual pivot))
            (pauli-mult residual row)
            residual)))
    (and (bvzero? (pauli-xs residual))
         (bvzero? (pauli-zs residual)))))
