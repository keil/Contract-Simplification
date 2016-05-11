#lang racket

;

;

;(define Baseline-reductionx
;  (extend-reduction-relation
;   Subset-reduction
;   λCon-Baseline
;   #:domain (ς M)
   

   
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
;   (--> (ς
;         (in-hole F (λ x (in-hole H (x @ ι I))))) ;; ? all contracts? ;; use special context
;        ;(((ι ◃ (ι1 ∩ ι2)) ς)
;        (ς
;         (in-hole F ((λ x (in-hole H x)) @ ι (I → ⊤))))
;        ; (in-hole F ((λ x (in-hole F (x @ ι2 ⊤))) @ ι1 (I → ⊤))))
;        "Lift"
 ;       (fresh ι1 ι2))
 ;  ))


