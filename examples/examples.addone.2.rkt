#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

;; AddOne
;; ======
;; Motivating Example.



;; # λJ (Reduction without contracts)
;; ----------------------------------

(define 
  example/addone/2
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y))))))

;(traces λCon-reduction (term (· (,example/addone/2 1))))

;; Notes
;; -----
;; Reduction steps: 6



;; # λCon (Reduction with contracts)
;; ---------------------------------

(define 
  example/addone/2/contracted
  (term ((λ f ((λ x ((f 1) x)) @ ♭0 (Number? → Number?))) ((λ x (λ y (+ x y))) @ ♭1 (Number? → (Number? → Number?))))))

;(traces λCon-reduction (term (· (,example/addone/2/contracted, 1))))

;; Notes
;; -----
;; Reduction steps: 36




;; # Sugar Reduction
;; -----------------

(traces Baseline-reduction (term (· ,example/addone/2/contracted)))

;; Notes
;; -----
;; Optimization steps: 20

(let ([configuration (λCon~~>* (term (· ,example/addone/2/contracted)))]) 
  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))

;; Notes
;; -----
;; Reduction steps: 31