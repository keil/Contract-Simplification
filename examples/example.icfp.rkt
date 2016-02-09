#lang racket
(require redex)
(require rackunit)

(require "lcon.rkt")

;; Blame-example from the ICFP paper
;; ---------------------------------
(traces
 λCon-reduction
 (term (· ((((λ f f) @ ♭ ((Positive? → Positive?) → Number?)) (λ x (- 0 1))) 1))))