#lang rosette

(require racket/string)

(provide ;; data structures
         (struct-out pauli1)
         (struct-out pauli)
         ;; constructors
         define-symbolic-pauli
         pauli-id
         symbol->pauli
         symbol->pauli1
         symbols->pauli
         ;; boolean validity of Paulis
         single-qubit-pauli?
         wf-pauli?
         ;; projectors
         pauli1-at
         pauli-weight
         ;; pretty-printing
         pauli1->symbol
         pauli->symbols
         pretty-pauli1
         pretty-pauli
         ;; examples
         example1
         example2)

(struct pauli1 (x z) #:transparent)
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
    [(pauli1-x p) 'X]
    [(pauli1-z p) 'Z]
    [else 'I]))

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
  (pauli n
         (bv (if (pauli1-x single-pauli) (arithmetic-shift 1 (- n i 1)) 0) n)
         (bv (if (pauli1-z single-pauli) (arithmetic-shift 1 (- n i 1)) 0) n)))

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

(define (pauli-id width)
  (pauli width (bv 0 width) (bv 0 width)))


(define (pauli-weight P)
  (define weight 0)
  (for ([i (in-range (pauli-width P))])
    (cond
      [(not (eq? (pauli1-at i P) (symbol->pauli1 'I)))
       (set! weight (add1 weight))]))
  weight)

;; Example use-case
(define example1 (pauli 2 (bv #b01 2) (bv #b10 2)))
(define example2 (symbols->pauli '(X Z)))
