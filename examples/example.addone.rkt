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
  example/addone/1
  (term ((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))))

;(traces λCon-reduction (term (· (,example/addone/1 1))))

;; Notes
;; -----
;; Reduction steps: 10



;; # λCon (Reduction with contracts)
;; ---------------------------------

(define 
  example/addone/1/contracted
  (term ((λ f (λ x ((f 1) x))) ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭ ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))))

;(traces λCon-reduction (term (· (,example/addone/1/contracted 1))))
  
;; Notes
;; -----
;; Reduction steps: 46



;; # Sugar Reduction
;; -----------------

;(traces Baseline-reduction (term (· ,example/addone/1/contracted)))

;; Notes
;; -----
;; Optimization steps: 30

(let ([configuration (λCon~~>* (term (· ,example/addone/1/contracted)))]) 
  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 1)))))

;; Notes
;; -----
;; Reduction steps: 38