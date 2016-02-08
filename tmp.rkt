#lang racket
(require redex)
(require rackunit)

(require "lcon.rkt")


(redex-match? 
 λCon M
 (term ((+ 1 2) @ ♭ (((Λ x (Λ y (flat (⊤ / (λ z (and (<= x z) (>= y z))))))) 1) 9))))

(redex-match? 
 λCon M
 (term ((+ 1 2) @ ♭ (((Λ x (Λ y (flat (⊤ / (λ z (and (<= x z) (>= y z))))))) 0) 9))))



(traces 
  λCon-reduction 
 (term (· ((+ 1 2) @ ♭ (((Λ x (Λ y (flat (⊤ / (λ z (and (<= x z) (>= y z))))))) 0) 9))))
 )




(test-results)