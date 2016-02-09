#lang racket
(require redex)
(require rackunit)

(require "lcon.rkt")

(redex-match? λCon M (term ((((λ f f) @ ♭ ((Positive? → Positive?) → Number?)) (λ x (- 0 1))) 1)))

(traces
 λCon-reduction
 (term (· ((((λ f f) @ ♭ ((Positive? → Positive?) → Number?)) (λ x (- 0 1))) 1))
  ))

;(redex-match? 
; λCon M
; (term ((+ 1 2) @ ♭ (flat %Positive %Even))))




(test-results)