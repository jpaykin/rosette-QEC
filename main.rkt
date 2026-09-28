#lang rosette
(require rosette/lib/synthax)
(require racket/string)

(struct pauli1 (x z) #:transparent)
(struct pauli (width xs zs) #:transparent)

(define (single-qubit-pauli? p)
  (and (pauli1? p)
       (boolean? (pauli1-x p))
       (boolean? (pauli1-z p))
  ))

(define (wf-pauli? P)
    (and (pauli? P)
       ((bitvector (pauli-width P)) (pauli-xs P))
       ((bitvector (pauli-width P)) (pauli-zs P))))



(define (bool-at i v)
  (bitvector->bool (bit i v)))

(define (pauli1-at i P)
    (pauli1 (bool-at i (pauli-xs P))
            (bool-at i (pauli-zs P)))
)

; Return a 1-bit bitvector
(define (bv-inner-product width u v)
  (for/fold ([parity (bv 0 (bitvector 1))])
            ([i (in-range width)])
    (bvxor  parity
            (bvand (bit i u)
                   (bit i v)))))


(define (commute? P1 P2)
  (define w (pauli-width P1))
  (assert (eq? (pauli-width P2) w))
  ; (displayln (list "commute? " P1 " " P2))
  ; (displayln (list "\t" (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
  ;                       "\t"
  ;                       (bv-inner-product w (pauli-zs P1) (pauli-xs P2))))
  ; (displayln (bvsub  (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
  ;           (bv-inner-product w (pauli-zs P1) (pauli-xs P2))))
  (bvzero?
    (bvsub  (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
            (bv-inner-product w (pauli-zs P1) (pauli-xs P2))))
  )

(define (wf-generators? gens)
  (and
    ;; gens should be non-empty
    (list? gens) (pair? gens)
    ;; every generator should be a valid Pauli of the same width
    (let ([n (pauli-width (first gens))])
      (for ([i (in-range (length gens))])
        (let ([Pi (list-ref gens i)])
          (and
            (wf-pauli? Pi)
            (= (pauli-width Pi) n)
          ))))
    ;; every pair of generators should commute with each otehr
    (for* ( [i (in-range (length gens))]
            [j (in-range (add1 i) (length gens))])
      (commute? (list-ref gens i) (list-ref gens j)))
  ))

(define-syntax (define-symbolic-pauli stx)
  (syntax-case stx ()
    [(_ name n)
     (with-syntax ([(xs zs) (generate-temporaries #'(name name))])
       #'(begin
           (define-symbolic xs (bitvector n))
           (define-symbolic zs (bitvector n))
           (define name (pauli n xs zs))))]))


(define (pauli1->symbol p)
   (cond
     [(and (pauli1-x p) (pauli1-z p)) 'Y]
     [(pauli1-x p)                    'X]
     [(pauli1-z p)                    'Z]
     [else                            'I]
   ))
  
(define (symbol->pauli1 p)
  (case p
    [(I) (pauli1 #f #f)]
    [(X) (pauli1 #t #f)]
    [(Z) (pauli1 #f #t)]
    [(Y) (pauli1 #t #t)]
    [else
     (raise-argument-error
      'symbol->pauli1
      "(or/c 'I 'X 'Y 'Z)"
      p)]))

; Input: a non-empty list of symbols 'I, 'X, 'Y, 'Z
; Output: an n-qubit Pauli operator
; Example: (symbols->pauli '(I X Z))
(define (symbols->pauli symbols)
  (unless (and (list? symbols) (pair? symbols))
    (raise-argument-error 'symbols->pauli "non-empty list?" symbols))

  (define width (length symbols))
  (define-values (xs zs)
    (for/fold ([xs 0] [zs 0])
              ([symbol (in-list symbols)])
      (define p (symbol->pauli1 symbol))
      (values (+ (* 2 xs) (if (pauli1-x p) 1 0))
              (+ (* 2 zs) (if (pauli1-z p) 1 0)))))

  (pauli width (bv xs width) (bv zs width)))


(define (pauli->symbols P)
  (unless (wf-pauli? P)
    (raise-argument-error 'pauli->symbols "wf-pauli?" P))

  (for/list ([i (in-range (sub1 (pauli-width P)) -1 -1)])
    (pauli1->symbol (pauli1-at i P))))

;; Pauli symplectic bit-pair convention: (x z) -> I/X/Z/Y.
(define (pretty-pauli1 p) (symbol->string (pauli1->symbol p)))
(displayln (pretty-pauli1 (pauli1 false true)))

(define (pretty-pauli P)
  (string-join
   (for/list ([i (in-range (sub1 (pauli-width P)) -1 -1)])
     (pretty-pauli1 (pauli1-at i P)))
   ""))


(define example1 (pauli 2 (bv #b01 2) (bv #b10 2)))
(displayln (pauli1-at 0 example1))
(displayln (pretty-pauli example1))

(define example2 (symbols->pauli '(X Z)))
(displayln (pretty-pauli example2))

(displayln (commute? (symbols->pauli '(X I)) (symbols->pauli '(I X))))



;; Read column 0..width-1 from X, then width..2*width-1 from Z.
(define (pauli-column-bit P column)
  (define width (pauli-width P))
  (if (< column width)
      (bit column (pauli-xs P))
      (bit (- column width) (pauli-zs P))))

;; XOR two Pauli vectors; this is Pauli multiplication modulo global phase.
(define (pauli-xor P1 P2)
  (pauli (pauli-width P1)
         (bvxor (pauli-xs P1) (pauli-xs P2))
         (bvxor (pauli-zs P1) (pauli-zs P2))))

;; Produce reduced row-echelon generator rows and their pivot columns.
;; Generator rows must be concrete and have the same width.
(define (pauli-row-echelon generators width)
  (define rows (list->vector generators))
  (define pivot-count 0)
  (define pivot-columns '())
  ;; Iterate over each column
  (for ([column (in-range (* 2 width))]
        #:break (= pivot-count (vector-length rows)))
      ;; A pivot is the first row with a 1 in the given column
      ;; Add the pivot row to every other row with a 1 in that column
      ;; Afterwards, only the pivot row has a 1 in that column
      ;; pivot-index will be #f if no pivot exists
    (define pivot-index
      (for/first ([candidate-index
                   (in-range pivot-count (vector-length rows))]
                  #:when
                  (bitvector->bool
                   (pauli-column-bit (vector-ref rows candidate-index)
                                     column)))
        candidate-index))

    ;; When the pivot index is not #f...
    (when pivot-index
      (define pivot (vector-ref rows pivot-index))
      (define displaced (vector-ref rows pivot-count))
      (vector-set! rows pivot-count pivot)
      (vector-set! rows pivot-index displaced)
      (for ([row-index (in-range (vector-length rows))]
            #:unless (= row-index pivot-count))
        (define row (vector-ref rows row-index))
        (when (bitvector->bool (pauli-column-bit row column))
          (vector-set! rows row-index (pauli-xor row pivot))))
      (set! pivot-columns (cons column pivot-columns))
      (set! pivot-count (add1 pivot-count))))
  (values (for/list ([row-index (in-range pivot-count)])
            (vector-ref rows row-index))
          (reverse pivot-columns)))

(define (print-generators generators)
  (for ([P (in-list generators)])
    (displayln (pretty-pauli P))))


(println "INITIAL GENERATORS")
(define gens
  (list (symbols->pauli '(Z X))
        (symbols->pauli '(Y Y))
  ))
(print-generators gens)
(println "REDUCED GENERATORS")
(let-values ([(basis pivots)
              (pauli-row-echelon gens
               2)])
  (print-generators basis)
)
(println "DONE TEST")

(define (pauli-in-group? P generators)
  (unless (and (wf-pauli? P)
               (exact-positive-integer? (pauli-width P)))
    (raise-argument-error 'pauli-in-group? "wf-pauli? with positive width" P))
  (unless (and (list? generators) (pair? generators))
    (raise-argument-error 'pauli-in-group? "non-empty list?" generators))
  (for ([generator (in-list generators)])
    (unless (and (= (pauli-width generator) (pauli-width P))
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
    ;; XOR each pivot row when target has a 1 in its pivot column.
    (define residual
      (for/fold ([residual P])
                ([pivot (in-list pivots)]
                 [row (in-list basis)])
        (if (bitvector->bool (pauli-column-bit residual pivot))
            (pauli-xor residual row)
            residual)))
    (and (bvzero? (pauli-xs residual))
         (bvzero? (pauli-zs residual)))))

(displayln '(pauli-in-group test))
(define generators
  (list (symbols->pauli '(Z X))
        (symbols->pauli '(Y Y))))
(print-generators generators)
(define membership-product
  (pauli-xor (first generators)
             (second generators)))
(unless (and (pauli-in-group? (symbols->pauli '(I I)) generators)
             (pauli-in-group? (first generators) generators)
             (pauli-in-group? membership-product generators)
             (not (pauli-in-group? (symbols->pauli '(X X))
                                   generators)))
  (error 'pauli-in-group-test "membership test failed"))
(displayln 'DONE)

(define-symbolic-pauli px 5)
(displayln px)

(define sol (solve (begin
      (assert (not (commute? px (symbols->pauli '(X Z I I X)))))
      (assert (not (eq? px (symbols->pauli '(I I I I I)))))
  )))
;;sol
(pauli->symbols (evaluate px sol))
