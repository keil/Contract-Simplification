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
  example/addone/3
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))))
;(traces λCon-reduction (term (· (,example/addone/1 1))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 58

(define 
  example/addone/3/contracted
  (term ((λ f ((λ x ((f 1) x)) @ ♭0 (Positive? → Positive?))) ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭1 ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))))
;(traces λCon-reduction (term (· (,example/addone/1/contracted 1))))
  

;; # Sugar Reduction
;; -----------------
;; Optimization steps: 21
;; Reduction steps:    35

;(traces Baseline-reduction (term (· ,example/addone/1/contracted)))

;(let ([configuration (λCon~~>* (term (· ,example/addone/1/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))



;; # Sugar Reduction
;; -----------------
;; Optimization steps: 39
;; Reduction steps:    34 (36)

(traces Subset-reduction (term (· ,example/addone/3/contracted)))
;(traces Finalize-reduction (λCon/Subset~~>* (term (· ,example/addone/3/contracted))))



;(let ([configuration (λCon/Finalize~~>* (term (· ,example/addone/3/contracted)))]) 
;  (traces Finalize-reduction (term ((stateOf ,configuration) ((termOf ,configuration) 1)))))


;(let ([configuration (λCon/Finalize~~>* (λCon/Subset~~>* (term (· ,example/addone/3/contracted))))]) 
;  (traces Finalize-reduction (term ((stateOf ,configuration) ((termOf ,configuration) 1)))))

;(let ([configuration (λCon/Finalize~~>* (λCon/Subset~~>* (term (· ,example/addone/3/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))