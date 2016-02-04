#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test λCon/ Syntax
;; =================

(check-eq?
 (redex-match? λCon M (term (1 @ (flat ⊤))))
 #t)

(check-eq?
 (redex-match? λCon M (term ((+ 1 2) @ (flat ⊤))))
 #t)

(check-eq?
 (redex-match? λCon M (term (1 @ (flat (⊤ / (λ x (+ 1 1)))))))
 #t)

(check-eq?
 (redex-match? λCon M (term ((+ 1 2) @ (flat (⊤ / (λ x (+ 1 1)))))))
 #t)

(check-eq?
 (redex-match? λCon M (term ((λ x (+ x 1)) @ ((flat ⊤) → (flat ⊤)))))
 #t)

(check-eq?
 (redex-match? λCon M (term (((λ x (+ x 1)) @ ((flat ⊤) → (flat ⊤))) 1)))
 #t)

(check-eq?
 (redex-match? λCon M (term ((λ x 1) 1)))
 #t)

(check-eq?
 (redex-match? λCon M (term ((λ x 1) #t)))
 #t)

;; Test λCon/ Reduction
;; ====================

(test-->> 
 λCon-reduction 
 (term (· ((+ 1 2) @ (flat ⊤))))
 (term (((♭ ◃ (#t ∘ #t)) ·) 3)))

(test-->> 
 λCon-reduction 
 (term (· ((+ 1 2) @ (flat ⊤))))
 (term (((♭ ◃ (#t ∘ #t)) ·) 3)))

(test-->> 
 λCon-reduction 
 (term (· ((+ 1 2) @ (flat %Boolean))))
 (term (((♭ ◃ (#t ∘ #f)) ·) 3)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x (+ x 1)) @ ((flat %Number) → (flat %Number))) 1)))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((♭ ◃ (ι1 → ι2)) ·))) 2)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x (+ x 1)) @ ((flat %Positive) → (flat %Positive))) 0)))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #f)) ((♭ ◃ (ι1 → ι2)) ·))) 1))) ;; TODO

(test-->> 
 λCon-reduction 
 (term (· (((λ x (- x 1)) @ ((flat %Positive) → (flat %Positive))) 1)))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((♭ ◃ (ι1 → ι2)) ·))) 0))) ;; TODO

(test-->> 
 λCon-reduction 
 (term (· ((((λ x (λ y (+ x y))) @ ((flat %Positive) → ((flat %Positive) → (flat %Positive)))) 1) 1)))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((♭ ◃ (ι1 → ι2)) ·))))) 2)))

(test-->> 
 λCon-reduction 
 (term (· ((λ x (x 1)) ((λ x (+ x 1)) @ ((flat %Positive) → (flat %Positive))))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((♭ ◃ (ι1 → ι2)) ·))) 2)))

(test-->> 
 λCon-reduction 
 (term (· ((((λ y (λ x ((y x) 1))) @ (((flat %Positive) → ((flat %Positive) → (flat %Positive))) → ((flat %Positive) → (flat %Positive)))) (λ x (λ y (+ x y)))) 1)))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι8 ◃ (#t ∘ #t)) ((ι7 ◃ (#t ∘ #t)) ((ι6 ◃ (ι7 → ι8)) ((ι5 ◃ (#t ∘ #t)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((♭ ◃ (ι1 → ι2)) ·))))))))) 2)))

(test-results)