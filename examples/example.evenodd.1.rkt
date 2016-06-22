#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")
(require "../subset.rkt")
(require "../join.rkt")
(require "../success.rkt")

(provide (all-defined-out))

;; Even-Odd (simple)
;; =================
;; Tests recursive contract checking.

;; # λJ (Reduction without contracts)
;; ----------------------------------
;; Reduction steps: 27

(define 
  example/evenodd/1
  (term 
   ;; even? x
   ((λ f g (λ x (f f g x)))
    ; even f
    (λ f g x (if (= x 0) #t (g f g (- x 1))))
    ; odd g
    (λ f g x (if (= x 0) #f (f f g (- x 1))))
    )))

;(traces λCon-reduction (term (· (,example/evenodd/1 3))))



;; # λCon (Reduction with contracts)
;; ---------------------------------
;; Reduction steps: 72

(define 
  example/evenodd/1/contracted
  (term
   ;; even? x
   ((λ f g (λ x (f f g x)))
    ; even f
    ((λ f g x (if (= x 0) #t (g f g (- x 1)))) @ ♭0 (⊤ ⊤ Natural? → Boolean?))
    ; odd g
    ((λ f g x (if (= x 0) #f (f f g (- x 1)))) @ ♭1 (⊤ ⊤ Natural? → Boolean?))
    )))

;(traces λCon-reduction (term (· (,example/evenodd/1/contracted 3))))




;; # Baseline Reduction
;; --------------------
;; Optimization steps: 13
;; Reduction steps:    68

;(traces Baseline-reduction (term (· ,example/evenodd/1/contracted)))

;(let ([configuration (λCon/Baseline~~>* (term (· ,example/evenodd/1/contracted)))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 3)))))



;; # Subset Reduction
;; ------------------
;; Optimization steps: 15
;; Join Steps:          0
;; Reduction steps:    68

;(traces Subset-reduction (term (· ,example/evenodd/1/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/evenodd/1/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Subset~~>* (term (· ,example/evenodd/1/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 3)))))




;; # Success Reduction
;; -------------------
;; Optimization steps: 23
;; Join Steps:          0
;; Reduction steps:    77

;(traces Success-reduction (term (· ,example/evenodd/1/contracted)))

;(traces Join-reduction (λCon/Subset~~>* (term (· ,example/evenodd/1/contracted))))

;(let ([configuration (λCon/Join~~>* (λCon/Success~~>* (term (· ,example/evenodd/1/contracted))))]) 
;  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 3)))))