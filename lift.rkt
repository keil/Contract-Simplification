#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
;(require "baseline.rkt")
(require "split.rkt")

(provide (all-defined-out))

#|
 ___          _                           _ 
/ __|_  _ _ _| |_ __ ___ __  __ _ _ _  __| |
\__ \ || | ' \  _/ _` \ \ / / _` | ' \/ _` |
|___/\_, |_||_\__\__,_/_\_\ \__,_|_||_\__,_|
     |__/                                   
 ___                     _   _      ___                 _         
/ __| ___ _ __  __ _ _ _| |_(_)__  |   \ ___ _ __  __ _(_)_ _  ___
\__ \/ -_) '  \/ _` | ' \  _| / _| | |) / _ \ '  \/ _` | | ' \(_-<
|___/\___|_|_|_\__,_|_||_\__|_\__| |___/\___/_|_|_\__,_|_|_||_/__/
                                                                  
|#

(define-extended-language λCon-Lift λCon-Baseline
  

  
  
  
  ;; Canonical terms (λJ terms)
  ;; --------------------------
  
  ;; Source Terms (Values)
  (S0
   K x (+blame ♭) (-blame ♭))
  
  ;; Source Terms (Abstractions)
  (S1 
   (λ x S))
  
  ;; Source Terms (Applications)
  (S2
   (S0 T) (TI T) (S S) (S1 TI))
  
  ;; Source Terms ()
  (S3
   (op T ...) (if T_0 T_1 T_2))
  
  ;; Source Terms (without contracts) 
  (S 
   S0 S1 S2 S3)
  
  ;; Terms with Immediate Contracts
  (TI
   (+blame ♭) (-blame ♭) S2 S3
   (TI @ ι I))
  
  ;; Terms with Delayed Contracts
  (TQ 
   S0 S1 TI (TQ @ ι Q))
  
  ;; Canonical Terms (non-reducable terms)
  (T S TI TQ (K @ ι ⊥) (x @ ι0 ι1)) ;; TODO
  
  
  ;; Reducable terms (non-cannonical terms)
  ;; --------------------------------------
  (Reducible
   
   ;; Terms containing a reducable term
   (λ x Reducible) (Reducible M) (M Reducible) (op M ... Reducible N ...) (if M ... Reducible N ...)   (Reducible @ b C)
   
   ;; Optimization
   ;; ------------
   
   ;; Delayed checkes of a delayed contract
   ((λ x M) (M @ ι Q))
   
   ;; Checked of delayed contracts
   ((M @ ι Q) N) 
   
   ;; Imediate contracts in values
   (K @ ι I) (x @ ι I) ((λ x M) @ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ι C))
   
   ;; Restructuring
   ;; -------------
   
   ;; Intersection betenn immediate and delayed contract
   (M @ ι (I ∩ Q))
   
   ;; Union contracts
   (M @ ι (C ∪ D))
   
   ;; Nested delayed contracts
   ((M @ ι_0 Q) @ ι_1 I)
   
   ;; Top-level assertions
   (T @ ♭ C))
  
  
  ;; Final Terms (top-level)
  ;; -----------------------
  (Final
   T (x @ ι C))
  
  )


#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Lift (one level)
;; ================
;; Verifies all (immediate) contracts that can be check at compile time
;; and unroll all intersection/union contracts.


(define Lift-reduction
  (extend-reduction-relation
   Baseline-reduction
   λCon-Lift
   #:domain (ς M)
   
   
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (ς
         (in-hole F (λ x (in-hole F0 (x @ ι I))))) ;; ? all contracts? ;; use special context
        ;        (((ι ◃ (ι1 ∩ ι2)) ς)
        ;(ς
        (((ι ◃ (ι1 → ι2)) ς)
         (in-hole F ((λ x (in-hole F0 x)) @ ι1 (I → ⊤))))
        ; (in-hole F ((λ x (in-hole H (x @ ι2 (⊥⊤)))) @ ι1 (I → ⊤))))
        "Lift/1"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (λ x (in-hole H (x @ ι I))))) ;; ? all contracts? ;; use special context
        ;        (((ι ◃ (ι1 ∩ ι2)) ς)
        ;(ς
        (((ι0 ◃ (ι1 → ι2)) ς)
         (in-hole F ((λ x (in-hole H x)) @ ι1 (I → ⊤))))
        ; (in-hole F ((λ x (in-hole H (x @ ι2 (⊥⊤)))) @ ι1 (I → ⊤))))
        "Lift/n"
        (fresh ι3 ι1 ι2))
   
   
   ))





#|
 ___            _ _         _                         _ 
| _ \_ _ ___ __| (_)__ __ _| |_ ___ ___  __ _ _ _  __| |
|  _/ '_/ -_) _` | / _/ _` |  _/ -_|_-< / _` | ' \/ _` |
|_| |_| \___\__,_|_\__\__,_|\__\___/__/ \__,_|_||_\__,_|
                                                        
 ___             _   _             
| __|  _ _ _  __| |_(_)___ _ _  ___
| _| || | ' \/ _|  _| / _ \ ' \(_-<
|_| \_,_|_||_\__|\__|_\___/_||_/__/
                                   
|#



;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Lift~~> ς M)
  (if (redex-match? λCon M M)
      (car (apply-reduction-relation Lift-reduction (term (,ς ,M))))
      (error "Invalid λCon-term:" M)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Lift~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Lift-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))