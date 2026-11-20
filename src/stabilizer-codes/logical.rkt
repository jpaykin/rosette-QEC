#lang rosette

(require "../paulis.rkt")

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
  (define (prop P #:except grp) (logical-error? P generators #:except grp))
  (synthesize-pauli width prop #:except seen)
  )

(define (find-logical-error-generators generators)
  (unless (wf-generators? generators)
    (raise-argument-error
      'find-logical-error-generators
      "wf-generators?"
      generators)
  )

  (define generators-reduced (reduce-generators generators))
  (define width (pauli-width (first generators)))
  (define (prop P #:except [grp '()]) (logical-error? P generators #:except grp))
  
  (synthesize-generators width prop)
)

(define (bits->bv n b ..)
  (append (bool->bitvector b)
          ..)
)

;; A decoder for r generators on n qubits should map an r-bit syndrome to
;; an n-qubit Pauli with that syndrome. This placeholder only delegates to
;; wf-generators?; it does not check the decoder.
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


(define (decoder-logical-error? P decoder generators #:except [except-arg '()])
  (and
    (logical-error?
      (pauli-mult
        (decoder (syndrome-of P generators))
        P)
      generators
      #:except except-arg)
    (not (member P except-arg))
  ))

(define (find-decoder-logical-error decoder generators #:exept [except-arg '()])
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
  (define (prop P #:except grp) (decoder-logical-error? P decoder generators #:except grp))

  (synthesize-pauli width prop #:except except-arg)
)

(define (find-decoder-logical-error-generators decoder generators)
  (unless (wf-decoder? decoder generators)
    (raise-argument-error 'find-decoder-logical-error-generators
    "wf-decoder?"
    decoder)
    )
  (define width (pauli-width (first generators)))
  (define (prop P #:except grp) (decoder-logical-error? P decoder generators #:except grp))
  (synthesize-generators width prop)
)