#lang racket
(require redex)

(require "../lcon.rkt")
;(require "../baseline.rkt")
;(require "../lift.rkt")
(require "../split.rkt")

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




;; # Sugar Reduction
;; -----------------
;; Optimization steps: 20
;; Reduction steps: 31

(traces Baseline-reduction (term (· ,example/addone/2/contracted)))

;; Notes
;; -----


;(let ([configuration (λCon~~>* (term (· ,example/addone/2/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))

;; Notes
;; -----
