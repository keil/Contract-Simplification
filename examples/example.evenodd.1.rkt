#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../join.rkt")

(provide (all-defined-out))

;; Even-Odd (simple)
;; =================
;; Tests recursive contract checking.

;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 27

(define 
  example/evenodd/0
  (term 
   ;; even? x
   (((λ f (λ g (λ x (((f f) g) x))))
     ; even f
     (λ f (λ g (λ x (if (= x 0) #t (((g g) f) (- x 1)))))))
    ; odd g
    (λ g (λ f (λ x (if (= x 0) #f (((f f) g) (- x 1)))))))
   ))

;(traces λCon-reduction (term (· (,example/evenodd/0 3))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 75

(define 
  example/evenodd/0/contracted
  (term
   ;; even? x
   (((λ f (λ g ((λ x (((f f) g) x)) @ ♭0 (Natural? → Boolean?)))))
     ; even f
     (λ f (λ g ((λ x (if (= x 0) #t (((g g) f) (- x 1)))) @ ♭0 (Natural? → Boolean?)))))
    ; odd g
    (λ g (λ f ((λ x (if (= x 0) #f (((f f) g) (- x 1)))) @ ♭1 (Natural? → Boolean?)))))
   ))

;(traces λCon-reduction (term (· (,example/evenodd/0/contracted 3))))




;; # Baseline Reduction
;; --------------------
;; Optimization steps: 20
;; Reduction steps:    77

;(traces Baseline-reduction (term (· ,example/evenodd/0/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/evenodd/0/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 3)))))



;; # Subset Reduction
;; ------------------
;; Optimization steps: 22
;; Join Steps:          0
;; Reduction steps:    77

;(traces Subset-reduction (term (· ,example/evenodd/0/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/evenodd/0/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Subset~~>* (term (· ,example/evenodd/0/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 3)))))