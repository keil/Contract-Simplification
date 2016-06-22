#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../join.rkt")
(require "../success.rkt")

(provide (all-defined-out))

;; AddOne (two contracts, intersection)
;; ====================================
;; Motivating Example.



;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 10

(define 
  example/addone/3
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))))

;(traces λCon-reduction (term (· (,example/addone/3 1))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 58

(define 
  example/addone/3/contracted
  (term ((λ f ((λ x ((f 1) x)) @ ♭0 (Positive? → Positive?))) ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭1 ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))))

;(traces λCon-reduction (term (· (,example/addone/3/contracted 1))))



;; # Baseline Reduction
;; --------------------
;; Optimization steps: 25
;; Reduction steps:    44

(traces Baseline-reduction (term (· ,example/addone/3/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/addone/3/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))



;; # Subset Reduction
;; ------------------
;; Optimization steps: 28
;; Join Steps:          4
;; Reduction steps:    23

;(traces Subset-reduction (term (· ,example/addone/3/contracted)))

(traces Join-reduction (λCon/Subset~~>* (term (· ,example/addone/3/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Subset~~>* (term (· ,example/addone/3/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))



;; # Success Reduction
;; -------------------
;; Optimization steps: 31
;; Join Steps:          0
;; Reduction steps:    32

;(traces Success-reduction (term (· ,example/addone/3/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/addone/3/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Success~~>* (term (· ,example/addone/3/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))