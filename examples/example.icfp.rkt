#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")

(provide (all-defined-out))

;; Blame-example from the ICFP paper
;; =================================

;; # 0
;; ---

(define 
  example/icfp/0
  (term (· ((((λ f f) @ ♭ ((Positive? → Positive?) → Number?)) (λ x (- 0 1))) 1))))

(traces λCon-reduction example/icfp/0)

;; # 1
;; ---

(define 
  example/icfp/1
  (term (· ((((λ f f) @ ♭ (((Positive? → Positive?) → Number?) ∪ (⊤ → ⊤))) (λ x (- 0 1))) 1))))

(traces λCon-reduction example/icfp/1)