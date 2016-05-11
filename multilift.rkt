#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "subset.rkt")

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

(define-extended-language λCon-Multilift λCon-Subset
  #|
  ;; Canonical terms (λJ terms)
  ;; ==========================
  
  ;; Source Terms
  ;; ------------
  ;; Terms without a contract on the outermost position.
  
  ;; Values
  (S0 K (λ x S))
  
  ;; Non-Values
  (S1 x (+blame ♭) (-blame ♭) (S TI) (TI T) (S S) (K T) (op T ...) (if T_0 T_1 T_2))
  
  ;; Source Terms
  (S S0 S1)
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  
  ;; Terms with Immediate Contracts/ False
  (TI S1 (TI @ ι I) (S @ ι ⊥))
  
  ;; Terms with Delayed Contracts
  (TQ S TI (TQ @ ι Q))
  
  ;; Canonical Terms (non-reducable terms)
  (T TQ)
  
  
  
  ;; Reducable terms (non-cannonical terms)
  ;; ======================================
  
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
   (K @ ι I) #| (x @ ι I) |# ((λ x M) @ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ι C))
   
   ;; True
   (M @ b ⊤)
   
   ;; Restructuring
   ;; -------------
   
   ;; Intersection betenn immediate and delayed contract
   (M @ ι (I ∩ C))
   
   ;; Union contracts
   (M @ ι (C ∪ D))
   
   ;; Nested delayed contracts
   ((M @ ι_0 Q) @ ι_1 I) ((M @ ι_0 C) @ ι_1 ⊥)
   
   ;; Top-level assertions
   (T @ ♭ C))
  |#
  
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

(define Multilift-reduction
  (extend-reduction-relation
   Subset-reduction
   λCon-Multilift
   #:domain (ς M)
   
   
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
;   (--> (ς
;         (in-hole F (λ x (in-hole BCtx (x @ ι I)))))
;        (((ι ◃ (¬ ι1)) ς)
;         (in-hole F ((λ x (in-hole BCtx x)) @ ι1 (I → ⊤))))
;        "Lift/One"
;        (fresh ι1)
;        (side-condition (canonical? (term (in-hole F (λ x (in-hole BCtx (x @ ι I)))))))
;        )
   
   (--> (ς
         (in-hole F (λ x (in-hole G (x @ ι I)))))
        (((ι ◃ (¬ ι1)) ς)
         (in-hole F ((λ x (in-hole G x)) @ ι1 (I → ⊤))))
        "Multilift"
        (fresh ι1)
        (side-condition (canonical? (term (in-hole F (λ x (in-hole BCtx (x @ ι I)))))))
        )
   
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
  (λCon/Multilift~~> ς M)
  (if (redex-match? λCon M M)
      (car (apply-reduction-relation Multilift-reduction (term (,ς ,M))))
      (error "Invalid λCon-term:" M)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Multilift~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Multilift-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))