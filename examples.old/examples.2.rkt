#lang racket
(require redex)

(require "../baseline.rkt")

(provide (all-defined-out))


;; Motivating Example: AddOne
;; ==========================

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (((λ f (f 1)) (λ x (λ y (+ x y)))) @ ι4 (Number? → Number?)))))

;(test-->>
; Baseline-reduction
; (term (· ((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
; (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f x) 1))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) ((λ f (λ x (((f (x @ ι1 Number?)) (1 @ ι3 Number?)) @ ι4 Number?))) (λ x (λ y (+ x y)))))))

;(test-->>
; Baseline-reduction
; (term (· (((λ f (λ x ((f 1) x))) @ ♭ ((Number? → (Number? → Number?)) → (Number? → Number?))) (λ x (λ y ;(+ x y))))))
; (term (· (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

;(test-->>
; Baseline-reduction
; (term (· (((λ f (λ x ((f 1) x))) @ ♭0 ((Number? → (Number? → Number?)) → (Number? → Number?))) ((λ x (λ ;y (+ x y))) @ ♭1 (Number? → (Number? → Number?))))))
; (term (· (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

;(test-->>
; Baseline-reduction
; (term (· ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ♭ ((Number? → (Number? → Number?)) ∩ (Number? → (Number? → ;Number?)))))))
; (term (((ι ◃ (ι5 → ι6)) ((ι ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))) ((λ f (((f ((1 @ ι3 ⊥) @ ι5 ;Number?)) @ ι6 (Number? → Number?)) @ ι4 (String? → String?))) (λ x (λ y (+ x y)))))))

;; XXX
;(test-->>
; Baseline-reduction
; (term (· ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ♭ ((Number? → (Number? → Number?)) ∩ (String? → (String? → ;String?)))))))
; (term (((ι ◃ (ι5 → ι6)) ((ι ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))) ((λ f (((f ((1 @ ι3 ⊥) @ ι5 Number?)) @ ι6 (Number? → Number?)) @ ι4 (String? → String?))) (λ x (λ y (+ x y)))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f x) x))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) ((λ f (λ x (((f (x @ ι1 Number?)) (x @ ι3 Number?)) @ ι4 Number?))) (λ x (λ y (+ x y)))))))





;; Motivating Example: Inc
;; =======================

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x (f x))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)) ((λ f (λ x ((f (x @ ι1 Number?)) @ ι2 (Number? → Number?)))) (λ x (λ y (+ x y)))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x (λ y ((f x) y)))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) ((λ f (λ x (λ y (((f (x @ ι1 Number?)) (y @ ι3 Number?)) @ ι4 Number?)))) (λ x (λ y (+ x y)))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x (λ y ((f y) x)))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) ((λ f (λ x (λ y (((f (y @ ι1 Number?)) (x @ ι3 Number?)) @ ι4 Number?)))) (λ x (λ y (+ x y)))))))


(test-results)