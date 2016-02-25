#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")


#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test: Simple Terms
;; ==================

(test-->>
 Baseline-reduction
 (term (· (((λ x (+ x 1)) @ ♭ (Number? → Number?)) 1)))
 (term (((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) (((λ x (+ x 1)) 1) @ ι2 Number?))))

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Note: Matthias Keil
; [Lower] moves only function/delayed contracts
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;(test-->>
; Baseline-reduction
; (term (· (λ x (((+ x 1) @ ♭ Number?) @ Positiv?))))
; (term (· (((λ x (+ x 1)) @ (⊤ → Number?)) @ (⊤ → Positiv?)))))

;(test-->>
; Baseline-reduction
; (term (· ((λ x (((+ x 1) @ ♭ Number?) @ Positiv?)) 1)))
; (term (· ((((λ x (+ x 1)) 1) @ Number?) @ Positiv?))))



;; Test Unroll
;; ===========

(test-->>
 Baseline-reduction
 (term (· ((λ x (+ 1 (x 2))) ((λ x (+ x 1)) @  ♭ (Number? → Number?)))))
 (term (((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) ((λ x (+ 1 ((x 2) @ ι2 Number?))) (λ x (+ x 1))))))

(test-->>
 Baseline-reduction
 (term (· ((λ x (+ (x 1) (x 2))) ((λ x (+ x 1)) @  ♭ (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) ((λ x (+ ((x 1) @ ι2 Number?) ((x 2) @ ι4 Number?))) (λ x (+ x 1))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x 1) @ ♭ (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (((λ f (f 1)) (λ x 1)) @ ι4 Number?))))

(test-->>
 Baseline-reduction
 (term (· ((λ x (x 1)) ((λ x x) @ ♭ (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (((λ x (x 1)) (λ x x)) @ ι4 Number?))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x x) @ ♭ ((Number? → Number?) → (Number? → Number?))))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) (((λ f (f (1 @ ι1 (Number? → Number?)))) (λ x x)) @ ι4 (Number? → Number?)))))






;; Test: Unfold
;; ============

(test-->>
 Baseline-reduction
(term (· ((λ x ((x @ ♭ (Number? → Number?)) 2)) (λ x (+ x 1)))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (((λ x (x 2)) (λ x (+ x 1))) @ ι4 Number?))))

(test-->>
 Baseline-reduction
 (term (· ((λ x (+ 1 ((x @ ♭ (Number? → Number?)) 2))) (λ x (+ x 1)))))
 (term (((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) ((λ x (+ 1 ((x 2) @ ι2 Number?))) (λ x (+ x 1))))))

(test-->>
 Baseline-reduction
 (term (· ((λ x (+ ((x @ ♭0 (Number? → Number?)) 1) ((x @ ♭1 (Number? → Number?)) 2))) (λ x (+ x 1)))))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (ι4 → ι5)) ((♭1 ◃ ι3) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭0 ◃ ι) ·)))))) ((λ x (+ ((x 1) @ ι2 Number?) ((x 2) @ ι5 Number?))) (λ x (+ x 1))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x (+ x 1)) @ ♭ (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (((λ f (f 1)) (λ x (+ x 1))) @ ι4 Number?))))



;; Test: Collapse
;; ==============



(test-results)