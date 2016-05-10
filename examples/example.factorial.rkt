#lang racket
(require redex)

(require "../lcon.rkt")
;(require "../baseline.rkt")
(require "../lift.rkt")
(require "../split.rkt")
(require "../subset.rkt")

(provide (all-defined-out))

;; Factorial (simple)
;; ==================
;; Tests recursive contract checking.

;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 31

(define 
  example/factorial/0
  (term ((λ f (λ x ((f f) x))) (λ f (λ x (if (= x 1) 1 (* x ((f f) (- x 1)))))))))
;(traces λCon-reduction (term (· (,example/factorial/0 5))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 91

(define 
  example/factorial/0/contracted
  (term ((λ f (λ x ((f f) x))) (λ f ((λ x (if (= x 1) 1 (* x ((f f) (- x 1))))) @ ♭ (Natural? → Positive?))))))
;(traces λCon-reduction (term (· (,example/factorial/0/contracted 5))))

;; # Sugar Reduction
;; -----------------
;; Optimization steps: 27
;; Reduction steps:    77

(traces Lift-reduction (term (· ,example/factorial/0/contracted)))

;(let ([configuration (λCon/Subset~~>* (term (· ,example/factorial/0/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 5)))))