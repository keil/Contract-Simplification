#lang racket

;(test-->> 
; λCon-reduction 
; (term (· ((+ 1 2) @ ♭ (((Λ x (Λ y (flat (λ z (and (<= x z) (>= y z)))))) 0) 9))))
; (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 3)))

;(test-->> 
; λCon-reduction 
; (term (· ((+ 1 2) @ ♭ (((Λ x (Λ y (flat (λ z (and (<= x z) (>= y z)))))) 0) 2))))
; (term (((ι ◃ (#t ∘ #f)) ((♭ ◃ ι) ·)) (+blame ♭))))


