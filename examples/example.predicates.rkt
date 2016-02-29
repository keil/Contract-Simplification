#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))


(define example/predicate/0
  (term (λ y (((λ x (+ x 1)) @ ♭0 (Positive? → Positive?)) (y @ ♭1 (flat (λ x (and (positive? x) (even? x)))))))))

;(traces λCon-reduction (term (· (,example/predicate/0 2))))

;(traces Baseline-reduction (term (· ,example/predicate/0)))

;(let ([configuration (λCon~~>* (term (· ,example/predicate/0)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))








(define example/predicate/1
  (term (λ y (((λ x (+ x 1)) @ ♭0 (Positive? → Positive?)) (y @ ♭1 (Positive? ∩ Even?))))))

;(traces λCon-reduction (term (· (,example/predicate/1 2))))

(traces Baseline-reduction (term (· ,example/predicate/1)))

(let ([configuration (λCon~~>* (term (· ,example/predicate/1)))]) 
  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))




