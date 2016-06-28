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
;; Reduction steps: 12

(define 
  example/addone/5
  (term ((λ f (λ x (if (string? x) ((f "1") x) ((f 1) x)))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))))

;(traces λCon-reduction (term (· (,example/addone/5 1))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 72

(define 
  example/addone/5/contracted
  (term ((λ f ((λ x (if (string? x) ((f "1") x) ((f 1) x))) @ ♭0 ((Positive? → Positive?) ∩ (String? → String?)))) ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭1 ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))))

;(traces λCon-reduction (term (· (,example/addone/5/contracted 1))))



;; # Baseline Reduction
;; --------------------
;; Optimization steps: 27
;; Reduction steps:    56

;(traces Baseline-reduction (term (· ,example/addone/5/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/addone/5/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))



;; # Subset Reduction
;; ------------------
;; Optimization steps: 37
;; Join Steps:         12
;; Reduction steps:    46

;(traces Subset-reduction (term (· ,example/addone/5/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/addone/5/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Subset~~>* (term (· ,example/addone/5/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))



;; # Success Reduction
;; -------------------
;; Optimization steps: xx
;; Join Steps:         xx
;; Reduction steps:    xx

;(traces Success-reduction (term (· ,example/addone/5/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/addone/5/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Success~~>* (term (· ,example/addone/5/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))