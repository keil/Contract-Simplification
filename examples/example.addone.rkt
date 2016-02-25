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
  (term (((λ f (λ x ((f 1) x)))
          (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))
         1)))

;; Notes
;; -----
;; Reduction steps: 10

;(traces λCon-reduction (term (· ,example/addone/0)))



;; # λCon (Reduction with contracts)
;; ---------------------------------

(define 
  example/addone/1
  (term (((λ f (λ x ((f 1) x)))
          ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭ ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))
         1)))

;; Notes
;; -----
;; Reduction steps: 46

;(traces λCon-reduction (term (· ,example/addone/1)))



;; # Sugar Reduction
;; -----------------

;(test-->>
; Baseline-reduction
; (term (· ,example/addone/1))
; (term (((ι12 ◃ (#t ∘ #t)) ((ι13 ◃ (#t ∘ #t)) ((ι10 ◃ (ι13 → ι14)) ((ι11 ◃ (#t ∘ #t)) ((ι8 ◃ (ι11 → ι12)) ((ι9 ◃ (#t ∘ #t)) ((ι4 ◃ (ι9 → ι10)) ((ι7 ◃ (#t ∘ #t)) ((ι5 ◃ (ι7 → ι8)) ((ι3 ◃ (ι5 ∩ ι6)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))))))))))) ((((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) 1) @ ι14 Number?))))

;; Notes
;; -----
;; Optimization steps: 23


;; Notes
;; -----
;; Reduction steps: 11

(traces Baseline-reduction (term (· ,example/addone/1)))
(traces λCon-reduction (λCon~~>* (term (· ,example/addone/1))))