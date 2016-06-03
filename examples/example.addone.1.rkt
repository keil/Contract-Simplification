#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../join.rkt")

(provide (all-defined-out))

;; AddOne (intersection)
;; =====================
;; Motivating Example.



;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 10

(define 
  example/addone/1
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))))

;(traces λCon-reduction (term (· (,example/addone/1 1))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 46

(define 
  example/addone/1/contracted
  (term ((λ f (λ x ((f 1) x))) ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭ ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))))

;(traces λCon-reduction (term (· (,example/addone/1/contracted 1))))
  


;; # Baseline Reduction
;; --------------------
;; Optimization steps: 21
;; Reduction steps:    33

;(traces Baseline-reduction (term (· ,example/addone/1/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/addone/1/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))


;; # Subset Reduction
;; ------------------
;; Optimization steps: XX
;; Join Steps:         XX
;; Reduction steps:    XX

(traces Subset-reduction (term (· ,example/addone/1/contracted)))

(traces Join-reduction (λCon/Subset~~>* (term (· ,example/addone/1/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Subset~~>* (term (· ,example/addone/0/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))