#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../join.rkt")
(require "../success.rkt")

(provide (all-defined-out))

;; Factorial (simple)
;; ==================
;; Tests recursive contract checking.

;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 26

(define 
  example/factorial/1
  (term ((λ f (λ x (f f x))) (λ f x (if (= x 1) 1 (* x (f f (- x 1))))))))

;(traces λCon-reduction (term (· (,example/factorial/1 5))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 87

(define 
  example/factorial/1/contracted
  (term ((λ f (λ x (f f x))) ((λ f x (if (= x 1) 1 (* x (f f (- x 1))))) @ ♭ (⊤ Natural? → Positive?)))))

;(traces λCon-reduction (term (· (,example/factorial/1/contracted 5))))



;; # Baseline Reduction
;; --------------------
;; Optimization steps:  9
;; Reduction steps:    85

;(traces Baseline-reduction (term (· ,example/factorial/1/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/factorial/1/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 5)))))



;; # Subset Reduction
;; ------------------
;; Optimization steps: 11
;; Join Steps:          0
;; Reduction steps:    85

;(traces Subset-reduction (term (· ,example/factorial/1/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/factorial/1/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Subset~~>* (term (· ,example/factorial/1/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 5)))))



;; # Success Reduction
;; -------------------
;; Optimization steps: 13
;; Join Steps:          0
;; Reduction steps:    90

(traces Success-reduction (term (· ,example/factorial/1/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/factorial/1/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Success~~>* (term (· ,example/factorial/1/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 5)))))