#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")


(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))










(define-metafunction λCon
  Σ : P -> (M ...)
  [(Σ ⊤) ((λ x #t))]
  [(Σ ⊥) ((λ x #f))]
  ;[(Σ (⊤ / M)) (M)]
  [(Σ (P / M)) (⊕ (Σ P) (M))]
  [(Σ predefined) (Σ (lookup/ predefined))]
  )


(define-metafunction λCon
  ⊕ : (M ...) (M ...) -> (M ...)
  [(⊕ (M ...) ()) (M ...)]
  [(⊕ () (M ...)) (M ...)]
  [(⊕ (M_0 ... M_n M_i ...) (M_n M_m ...)) (⊕ (M_0 ... M_n M_i ...) (M_m ...))]
  [(⊕ (M_0 ...) (M_n M_m ...)) (⊕ (M_0 ... M_n) (M_m ...))])




