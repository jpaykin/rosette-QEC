#lang rosette

(require racket/string)

(provide (all-defined-out))

; A single-qubit (unsigned) Pauli is a pair of bits X^x Z^z
(struct pauli1 (x z) #:transparent)
; An n-qubit Pauli (where n is referred to as the width)
; is a pair of size-n bitvectors xs and zs
(struct pauli (width xs zs) #:transparent)

;; Validity

(define (single-qubit-pauli? p)
  (and (pauli1? p)
       (boolean? (pauli1-x p))
       (boolean? (pauli1-z p))))

(define (wf-pauli? P)
  (and (pauli? P)
       ((bitvector (pauli-width P)) (pauli-xs P))
       ((bitvector (pauli-width P)) (pauli-zs P))))

(define (bool-at i v)
  (bitvector->bool (bit i v)))

(define (pauli1-at i P)
  (pauli1 (bool-at i (pauli-xs P))
          (bool-at i (pauli-zs P))))

;; Usage: (define-symbolic-pauli1 name)
(define-syntax (define-symbolic-pauli1 stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([(x z) (generate-temporaries #'(name name))])
       #'(begin
           (define-symbolic* x boolean?)
           (define-symbolic* z boolean?)
           (define name (pauli1 x z))))]))

;; Usage: (define-symbolic-pauli name n)
(define-syntax (define-symbolic-pauli stx)
  (syntax-case stx ()
    [(_ name n)
     (with-syntax ([(xs zs) (generate-temporaries #'(name name))])
       #'(begin
           (define-symbolic* xs (bitvector n))
           (define-symbolic* zs (bitvector n))
           (define name (pauli n xs zs))))]))

(define (bits->pauli n i bx bz)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'bits->pauli "exact-positive-integer?" n))
  (unless (and (exact-nonnegative-integer? i) (< i n))
    (raise-argument-error
     'bits->pauli
     (format "exact integer in [0, ~a)" n)
     i))
  (unless (boolean? bx)
    (raise-argument-error 'bits->pauli "boolean?" bx))
  (unless (boolean? bz)
    (raise-argument-error 'bits->pauli "boolean?" bz))

  (define mask (arithmetic-shift 1 (- n i 1)))
  (pauli n
         (if bx (bv mask n) (bv 0 n))
         (if bz (bv mask n) (bv 0 n))))

(define (pauli1->symbol p)
  (cond
    [(and (pauli1-x p) (pauli1-z p)) 'Y]
    [(pauli1-x p) 'X]
    [(pauli1-z p) 'Z]
    [else 'I]))

(define pauli1X (pauli1 #t #f))
(define pauli1Y (pauli1 #t #t))
(define pauli1Z (pauli1 #f #t))
(define pauli1I (pauli1 #f #f))

(define (symbol->pauli1 p)
  (case p
    [(I) pauli1I]
    [(X) pauli1X]
    [(Z) pauli1Z]
    [(Y) pauli1Y]
    [else
     (raise-argument-error
      'symbol->pauli1
      "(or/c 'I 'X 'Y 'Z)"
      p)]))


;; Convert an 'I, 'X, 'Y, or 'Z symbol to P_i
;; as an n-qubit Pauli
(define (symbol->pauli n p1 i)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'symbol->pauli "exact-positive-integer?" n))
  (unless (and (exact-nonnegative-integer? i) (< i n))
    (raise-argument-error
     'symbol->pauli
     (format "exact integer in [0, ~a)" n)
     i))
  (define single-pauli (symbol->pauli1 p1))
  (bits->pauli n i
               (pauli1-x single-pauli)
               (pauli1-z single-pauli)))

;; Convert a non-empty list of 'I, 'X, 'Y, and 'Z symbols to a Pauli in list order.
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

(define (pretty-pauli1 p)
  (symbol->string (pauli1->symbol p)))

(define (pretty-pauli P)
  (string-join
   (for/list ([i (in-range (sub1 (pauli-width P)) -1 -1)])
     (pretty-pauli1 (pauli1-at i P)))
   ""))


; Identity Pauli for n qubits
(define (pauli-id n)
  (pauli n (bv 0 n) (bv 0 n)))

(define (pauli-weight P)
  (for/sum ([i (in-range (pauli-width P))])
    (define p (pauli1-at i P))
    (if (or (pauli1-x p) (pauli1-z p))
        1
        0)))



;; Example use-case
; (define example1 (pauli 2 (bv #b01 2) (bv #b10 2)))
; (define example2 (symbols->pauli '(X Z)))
