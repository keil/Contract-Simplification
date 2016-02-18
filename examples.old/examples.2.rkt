#lang racket
(require redex)

(require "../baseline.rkt")

(provide (all-defined-out))


;; Motivating Example: AddOne
;; ==========================

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· (((λ f (f 1)) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f x) 1))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· (((λ f (λ x ((f x) 1))) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· (((λ f (λ x ((f 1) x))) @ ((Number? → (Number? → Number?)) → (Number? → Number?))) (λ x (λ y (+ x y))))))
 (term (· (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· (((λ f (λ x ((f 1) x))) @ ((Number? → (Number? → Number?)) → (Number? → Number?))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ((Number? → (Number? → Number?)) ∩ (Number? → (Number? → Number?)))))))
 (term (· (((λ f (f 1)) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))))
 (term (· ((λ f ((f (1 @ (String? • Number?))) @ ((String? • Number?) → (Number? • String?)))) (λ x (λ y (+ x y)))))))


(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x ((f x) x))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· (((λ f (λ x ((f x) x))) (λ x (λ y (+ x y)))) @ (Number? → Number?)))))





;; Motivating Example: Inc
;; =======================

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x (f x))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· (((λ f (λ x (f x))) (λ x (λ y (+ x y)))) @ (Number? → (Number? → Number?))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x (λ y ((f x) y)))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· ((λ f (λ x (λ y ((f (x @ Number?)) y)))) ((λ x (λ y (+ x y))) @ (⊤ → (Number? → Number?)))))))

(test-->>
 Baseline-reduction
 (term (· ((λ f (λ x (λ y ((f y) x)))) ((λ x (λ y (+ x y))) @ (Number? → (Number? → Number?))))))
 (term (· ((λ f (λ x (λ y ((f y) (x @ Number?))))) ((λ x (λ y (+ x y))) @ (⊤ → (Number? → Number?)))))))


(test-results)