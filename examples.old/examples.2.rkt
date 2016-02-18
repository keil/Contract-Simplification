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

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι9 ◃ (#t ∘ #t)) ((ι7 ◃ (#t ∘ #t)) ((ι4 ◃ (ι9 → ι10)) ((ι5 ◃ (ι7 → ι8)) ((ι3 ◃ (ι5 ∩ ι6)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))))))) ((((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ ι10 (⊤ → Number?)) @ ι8 (Number? → ⊤)))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f x) 1))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
 (term (((ι9 ◃ (#t ∘ #t)) ((ι7 ◃ (#t ∘ #t)) ((ι6 ◃ (ι9 → ι10)) ((ι3 ◃ (ι7 → ι8)) ((ι5 ◃ (#t ∘ #t)) ((ι2 ◃ (ι5 → ι6)) ((ι1 ◃ (ι3 ∩ ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))))))) ((((λ f (λ x ((f x) 1))) (λ x (λ y (+ x y)))) @ ι10 (⊤ → Number?)) @ ι8 (Number? → ⊤)))))

(test-->>
 Baseline-reduction
 (term (· (((λ f (λ x ((f 1) x))) @ ♭ ((Number? → (Number? → Number?)) → (Number? → Number?))) (λ x (λ y (+ x y))))))
 (term (((ι11 ◃ (#t ∘ #t)) ((ι9 ◃ (#t ∘ #t)) ((ι6 ◃ (ι11 → ι12)) ((ι7 ◃ (ι9 → ι10)) ((ι5 ◃ (ι7 ∩ ι8)) ((ι4 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #t)) ((ι1 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))))))) (((((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ ι12 (⊤ → Number?)) @ ι10 (Number? → ⊤)) @ ι2 (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· (((λ f (λ x ((f 1) x))) @ ♭0 ((Number? → (Number? → Number?)) → (Number? → Number?))) ((λ x (λ y (+ x y))) @ ♭1 (Number? → (Number? → Number?))))))
 (term (((ι10 ◃ (ι14 ∩ ι15)) ((ι6 ◃ (ι12 ∩ ι13)) ((ι9 ◃ (ι10 → ι11)) ((ι8 ◃ (#t ∘ #t)) ((ι3 ◃ (ι8 → ι9)) ((ι5 ◃ (ι6 → ι7)) ((ι4 ◃ (#t ∘ #t)) ((ι1 ◃ (ι4 → ι5)) ((♭1 ◃ ι3) ((ι ◃ (ι1 → ι2)) ((♭0 ◃ ι) ·))))))))))) (((λ f (((((λ x ((f 1) x)) @ ι11 (⊤ → Number?)) @ ι14 (Number? → ⊤)) @ ι7 (⊤ → Number?)) @ ι12 (Number? → ⊤))) (λ x (λ y (+ x y)))) @ ι2 (Number? → Number?)))))
  
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