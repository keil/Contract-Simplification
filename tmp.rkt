#lang racket
(require redex)
(require rackunit)

(require "lcon.rkt")

(test-->> 
 λCon-reduction 
 (term (· (((λ x (+ x 1)) @ ((flat %Positive) → (flat %Positive))) 0)))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #f)) ((♭ ◃ (ι1 → ι2)) ·))) 1))) ;; TODO


(term
 (is-blame-state? ((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #f)) ((♭ ◃ (ι1 → ι2)) ·))))
 )

(term
 (is-false? (μ ((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((♭ ◃ (ι1 → ι2)) ·))) ι2))
 )


;(term
; (in-blame-state? ((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #f)) ((♭ ◃ (ι1 → ι2)) ·))) (♭1 ♭2 ♭))
; )

(not (term
 (is-blame-state? ·)))

(test-results)