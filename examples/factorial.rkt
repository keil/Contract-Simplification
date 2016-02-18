#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

;; Factorial
;; =========
;; Tests recursive contract checking.

(define 
  example/factorial/0
  (term (· (((λ f (λ x ((f f) x))) (λ f ((λ x (if (= x 1) 1 (* x ((f f) (- x 1))))) @ ♭ (Natural? → Positive?)))) 5))))

(traces λCon-reduction example/factorial/0)