#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../lift.rkt")
(require "../multilift.rkt")

(provide (all-defined-out))

;; AddOne
;; ======
;; Motivating Example.



;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 6

(define 
  example/addone/2
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y))))))
;(traces λCon-reduction (term (· (,example/addone/2 1))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 36

(define 
  example/addone/2/contracted
  (term ((λ f ((λ x ((f 1) x)) @ ♭0 (Positive? → Positive?))) ((λ x (λ y (+ x y))) @ ♭1 (Number? → (Number? → Number?))))))
;(traces λCon-reduction (term (· (,example/addone/2/contracted, 1))))




;; # Baseline Reduction
;; --------------------
;; Optimization steps: 16
;; Reduction steps: 28

;(traces Baseline-reduction (term (· ,example/addone/2/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/addone/2/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))

;; # Subset Reduction
;; ------------------
;; Optimization steps: 17
;; Reduction steps: 22

;(traces Subset-reduction (term (· ,example/addone/2/contracted)))

;(let ([configuration (λCon/Subset~~>* (term (· ,example/addone/2/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))

;; # Subset Reduction
;; ------------------
;; Optimization steps: 17
;; Reduction steps: 17

(traces Lift-reduction (term (· ,example/addone/2/contracted)))

;(let ([configuration (λCon/Lift~~>* (term (· ,example/addone/2/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))


