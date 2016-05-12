#lang racket
(require redex)

(require "../lcon.rkt")
(require "../symbolic.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../lift.rkt")
(require "../multilift.rkt")

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
;; Optimization steps: 25
;; Reduction steps:    33

(traces Baseline-reduction (term (· ,example/addone/1/contracted)))

(let ([configuration (λCon/Finalize~~>* (λCon/Baseline~~>* (term (· ,example/addone/1/contracted))))]) 
  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))

;; # Lift Reduction
;; ----------------
;; Optimization steps: 33
;; Reduction steps:    35

;(traces Lift-reduction (term (· ,example/addone/1/contracted)))

;(let ([configuration (λCon/Lift~~>* (term (· ,example/addone/1/c;ontracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))