#lang rosette

(require "pauli.rkt"
         "algebra.rkt")

(provide logical-error?
         find-logical-error
         find-logical-error-generators
         
         five-qubit-code
         tiny-example-code
         repetition-code
         repetition-code-decoder

         define-symbolic-syndrome
         decoder-logical-error?
         find-decoder-logical-error
         find-decoder-logical-error-generators
         )

(define (logical-error? P generators)
  (&& (not (pauli-in-group? P generators))
      (andmap (lambda (Q) (commute? P Q)) generators)))

(define (find-logical-error generators)
  (unless (wf-generators? generators)
    (raise-argument-error
      'find-logical-error
      "wf-generators?"
      generators)
  )

  (define width (pauli-width (first generators)))
  (define-symbolic-pauli symbolic-p width)
  (define sol (solve (assert (logical-error? symbolic-p generators))))
  (if (unsat? sol)
      (error "Could not find a logical error")
      (begin
        (define concrete-p (evaluate symbolic-p sol))
        ;(println (pretty-pauli concrete-p))
        concrete-p)))

(define five-qubit-code
  (list (symbols->pauli '(X Z Z X I))
        (symbols->pauli '(I X Z Z X))
        (symbols->pauli '(X I X Z Z))
        (symbols->pauli '(Z X I X Z))))

(define tiny-example-code
  (list (symbols->pauli '(X Z Z X I))))

(define repetition-code
  (list (symbols->pauli '(Z Z I))
        (symbols->pauli '(I Z Z))))

(define shor-nine-qubit-code
  (list
    (symbols->pauli '(Z Z I I I I I I I))
    (symbols->pauli '(I Z Z I I I I I I))
    (symbols->pauli '(I I I Z Z I I I I))
    (symbols->pauli '(I I I I Z Z I I I))
    (symbols->pauli '(I I I I I I Z Z I))
    (symbols->pauli '(I I I I I I I Z Z))
    (symbols->pauli '(X X X X X X I I I))
    (symbols->pauli '(I I I X X X X X X))))


(define (find-logical-error-generators generators)
  (unless (wf-generators? generators)
    (raise-argument-error
      'find-logical-error-generators
      "wf-generators?"
      generators)
  )

  (define width (pauli-width (first generators)))
  ;(print "Width: ")
  ;(println width)

  ;(print "num-log-errors: ")
  ;(println num-log-errors)

  (let-values ([(generators-reduced _)
                (pauli-row-echelon generators width)])
    ;(print "reduced form: ")
    ;(print-paulis generators_reduced)

    (define num-log-errors (- width (length generators-reduced)))

    (print "num-log-errors: ")
    (println (- width (length generators-reduced)))


    (define errors (make-vector num-log-errors (pauli-id width)))
    (for ([i (in-range num-log-errors)])
      (define new-generators (append generators-reduced (vector->list errors)))
      ;(println "New generators found:")
      ;(print-paulis new-generators)
      (define P (find-logical-error new-generators))
      (vector-set! errors i P))

    (vector->list errors)))

(define (bits->bv n b ..)
  (concat (bool->bitvector b)
          ..)
)

;; Takes as input a 2-bit syndrome, returns a 3-qubit Pauli
(define (bool->bv1 b)
  (if b (bv 1 1) (bv 0 1)))

;; Need to be careful how you define this
;; so it produces a symbolic pauli
;; e.g. can't use symbol->pauli

(define (repetition-code-decoder syndrome)
  ( unless (and (list? syndrome) 
                (eq? (length syndrome) 2)
                (boolean? (first syndrome))
                (boolean? (second syndrome)))
      (raise-argument-error
        'repetition-code-decoder
        "ill-formed syndrome"
        syndrome))

  (define b0 (first syndrome))
  (define b1 (second syndrome))

  (define x0 (&& b0 (not b1)))
  (define x1 (&& b0 b1))
  (define x2 (&& (not b0) b1))

  (pauli 3
         (concat (bool->bv1 x0)
                 (bool->bv1 x1)
                 (bool->bv1 x2))
         (bv 0 3)))



  ; (let* ([b0 (first syndrome)]
  ;        [b1 (second syndrome)])
  ; (cond 
  ;   [(and (not b0) (not b1)) => (symbols->pauli '(I I I))]
  ;   [(and (    b0) (not b1)) => (symbols->pauli '(X I I))]
  ;   [(and (not b0) (    b1)) => (symbols->pauli '(I I X))]
  ;   [(and (    b0) (    b1)) => (symbols->pauli '(I X I))]
  ; )))

;; A decoder is well-formed with respect to an r-length
;; generator list of n-qubit Paulis
;; if it takes in a r-bit syndrome and returns a Pauli of size (n-r)
;;
;; it should also satisfy the property that,
;; for all inputs ,....
(define (wf-decoder? dec generators)
  (define width (pauli-width (first generators)))
  (wf-generators? generators)
  ; (and (wf-generators? generators)
  ;      (andmap 
  ;       (lambda (synd)
  ;         (and (wf-pauli? (dec synd))
  ;             (eq? (pauli-width (dec synd)) width)
  ;             ;(eq? (syndrome-of (dec synd) generators) synd)
  ;         ))
  ;       generators)
  )

(define (define-symbolic-syndrome len)
  (unless (exact-nonnegative-integer? len)
    (raise-argument-error 'define-symbolic-syndrome
                          "exact-nonnegative-integer?"
                          len))
  (for/list ([i (in-range len)])
    (define-symbolic* bit boolean?)
    bit))


(define (decoder-logical-error? P decoder generators except-arg)
  (and
    (logical-error?
      (pauli-mult
        (decoder (syndrome-of P generators))
        P)
      (append generators except-arg))
    (not (member P except-arg))
  ))

(define (find-decoder-logical-error decoder generators except-arg)
  (unless (wf-decoder? decoder generators)
    (raise-argument-error
      'find-decoder-logical-error
      "wf-decoder?"
      decoder
      generators
    ))
  (unless (wf-generators? (append generators except-arg))
    (raise-argument-error
      'find-decoder-logical-error
      "wf-generators?"
      generators
      except-arg))

  (define width (pauli-width (first generators)))
  (define-symbolic-pauli symbolic-p width)

  (define sol (solve (assert (decoder-logical-error? symbolic-p decoder generators except-arg))))
  (if (unsat? sol)
      #f
      (evaluate symbolic-p sol)
  ))

(define (find-decoder-logical-error-generators decoder generators)
  (unless (wf-decoder? decoder generators)
    (raise-argument-error 'find-decoder-logical-error-generators
    "wf-decoder?"
    decoder)
    )

  (define width (pauli-width (first generators)))
 
  (define (loop errors)
    (define P (find-decoder-logical-error decoder generators errors))
    (if (eq? P #f)
      errors
      (loop (cons P errors))
    ))
  (loop '())
)