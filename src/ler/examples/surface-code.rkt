
#lang rosette

(require "pauli.rkt"
         "algebra.rkt")

(provide surface-code)

;; The surface code acts on a dxd grid,
;; with boundaries
;;
;;   1,1 --- 1,2 --- ... --- 1,d
;;    |       |      |  |     |
;;   2,1 --- 2,2 --- ... --- 2,d
;;    |       |      |  |     |
;;   ...
;;    |       |      |  |     |
;;   d,1 --- d,2 --- ... --- d,d

;; every boundary and face in the grid above
;; corresponds to 2 stabilizer generators

