#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")

(provide (all-defined-out))

;; AddOne (simple)
;; ===============
;; Motivating Example.



;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 6

(define 
  example/addone/0
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y))))))
;(traces λCon-reduction (term (· (,example/addone/0 1))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 24

(define 
  example/addone/0/contracted
  (term ((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ ♭ (Number? → (Number? → Number?))))))
;(traces λCon-reduction (term (· (,example/addone/0/contracted, 1))))



;; # Baseline Reduction
;; --------------------
;; Optimization steps: 12
;; Reduction steps:    17

;(traces Baseline-reduction (term (· ,example/addone/0/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/addone/0/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))



;; # Subset Reduction
;; ------------------
;; Optimization steps: 16
;; Reduction steps:    18 (!)

;(traces Subset-reduction (term (· ,example/addone/0/contracted)))

;(let ([configuration (λCon/Subset~~>* (term (· ,example/addone/0/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))