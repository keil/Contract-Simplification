#lang racket
(require redex)

(require "../lcon.rkt")
;(require "../baseline.rkt")
(require "../lift.rkt")
(require "../split.rkt")
(require "../subset.rkt")

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
   (((λ f (λ g (λ x (((f f) g) x))))
     ; even f
     (λ f (λ g ((λ x (if (= x 0) #t (((g g) f) (- x 1)))) @ ♭0 (Natural? → Boolean?)))))
    ; odd g
    (λ g (λ f ((λ x (if (= x 0) #f (((f f) g) (- x 1)))) @ ♭1 (Natural? → Boolean?)))))
   ))
;(traces λCon-reduction (term (· (,example/evenodd/0/contracted 3))))

;; # Sugar Reduction
;; -----------------
;; Optimization steps: 27
;; Reduction steps:    77

(traces Lift-reduction (term (· ,example/evenodd/0/contracted)))

(let ([configuration (λCon/Subset~~>* (term (· ,example/evenodd/0/contracted)))]) 
  (traces λCon-reduction (term ((⇓/State ,configuration) ((⇓/Term ,configuration) 3)))))