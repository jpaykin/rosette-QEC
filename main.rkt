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

(define ((pauli-of-width? n) P)
    (and (pauli? P)
       (eq? n (pauli-width P))
       ((bitvector n) (pauli-xs P))
       ((bitvector n) (pauli-zs P))))
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

(define (bool-at i v)
  (bitvector->bool (bit i v)))

(define (pauli1-at i P)
    (pauli1 (bool-at i (pauli-xs P))
            (bool-at i (pauli-zs P)))
)

(define example1 (pauli 2 (bv #b01 2) (bv #b10 2)))
(displayln (pauli1-at 0 example1))
(displayln (pretty-pauli example1))

(define example2 (symbols->pauli '(X Z)))
(displayln (pretty-pauli example2))

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
  (displayln (list "commute? " P1 " " P2))
  (displayln (list "\t" (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
                        "\t"
                        (bv-inner-product w (pauli-zs P1) (pauli-xs P2))))
  (displayln (bvsub  (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
            (bv-inner-product w (pauli-zs P1) (pauli-xs P2))))
  (bvzero?
    (bvsub  (bv-inner-product w (pauli-xs P1) (pauli-zs P2))
            (bv-inner-product w (pauli-zs P1) (pauli-xs P2))))
  )

(displayln (commute? (symbols->pauli '(X I)) (symbols->pauli '(I X))))

(define-symbolic-pauli px 5)
(displayln px)

(define sol (solve (begin
      (assert (not (commute? px (symbols->pauli '(X Z I I X)))))
      (assert (not (eq? px (symbols->pauli '(I I I I I)))))
  )))
sol
(pauli->symbols (evaluate px sol))
