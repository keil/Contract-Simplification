#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

;; AddOne
;; ======
;; Motivating Example.



;; # λJ (Reduction without contracts)
;; ----------------------------------

(define 
  example/addone/0
  (term (· (((λ f (λ x ((f 1) x)))
             (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))
            1))))

;; Notes
;; -----
;; Reduction steps: 10

;(traces λCon-reduction example/addone/0)



;; # λCon (Reduction with contracts)
;; ---------------------------------

(define 
  example/addone/1
  (term (· (((λ f (λ x ((f 1) x)))
             ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭ ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))
            1))))

;; Notes
;; -----
;; Reduction steps: 46

;(traces λCon-reduction example/addone/1)



;; # Sugar Reduction
;; -----------------

(test-->>
 Baseline-reduction
 example/addone/1
 (term (((ι9 ◃ (#t ∘ #t)) ((ι7 ◃ (#t ∘ #t)) ((ι6 ◃ (ι9 → ι10)) ((ι3 ◃ (ι7 → ι8)) ((ι5 ◃ (#t ∘ #t)) ((ι2 ◃ (ι5 → ι6)) ((ι1 ◃ (ι3 ∩ ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))))))) ((((λ f (λ x ((f x) 1))) (λ x (λ y (+ x y)))) @ ι10 (⊤ → Number?)) @ ι8 (Number? → ⊤)))))

;; Notes
;; -----
;; Reduction steps: 46

(traces Baseline-reduction example/addone/1)