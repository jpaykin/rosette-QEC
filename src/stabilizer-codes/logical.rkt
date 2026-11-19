#lang rosette

(require "../paulis/definitions.rkt"
         "../paulis/algebra.rkt")

(provide (all-defined-out))

;; syndrome of a Pauli P with respect to a list of generators of a Pauli group
(define (syndrome-of P G)
  (map (lambda (g) (not (commute? P g))) G)
)

(define (logical-error? P generators #:except [seen '()])
  (&& (not (pauli-in-group? P (append generators seen)))
      (andmap (lambda (Q) (commute? P Q)) generators)))

(define (find-logical-error generators #:except [seen '()])
  (unless (wf-generators? generators)
    (raise-argument-error
      'find-logical-error
      "wf-generators?"
      generators)
  )

  (define width (pauli-width (first generators)))
  (define-symbolic-pauli symbolic-p width)
  (define sol (solve (assert (logical-error? symbolic-p generators #:except seen))))
  (if (unsat? sol)
      #f
      (begin
        (define concrete-p (evaluate symbolic-p sol))
        ;(println (pretty-pauli concrete-p))
        concrete-p)))

(define (find-logical-error-generators generators)
  (unless (wf-generators? generators)
    (raise-argument-error
      'find-logical-error-generators
      "wf-generators?"
      generators)
  )

  (define generators-reduced (reduce-generators generators))
  
  (define (loop errors)
    (define P (find-logical-error generators-reduced #:except errors))
    (if (eq? P #f)
      errors
      (loop (cons P errors))
    ))
  (loop '())
)

(define (bits->bv n b ..)
  (append (bool->bitvector b)
          ..)
)

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
      generators
      #:except except-arg)
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