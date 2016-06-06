#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

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

(define-extended-language λCon-Baseline λCon
  
  ;; Syntax Extensions
  ;; =================
  
  ;; Terms
  ;; -----
  ((L M N) .... (M @ ι C))
  
  
  
  ;; Canonical terms (λJ terms)
  ;; ==========================
  
  ;; Source Terms
  ;; ------------
  ;; Terms without a contract on the outermost position.
  
  ;; Values
  (SVal
   K (λ x S))
  
  ;; Non-Values
  (SNonVal
   x (blame ♭)
   (TI TQ) (TCons TQ) (TAbs TI) (TAbs TVal)
   (op TQ ...) (if TQ_0 TQ_1 TQ_2))
  
  ;; Source Terms
  (S SVal SNonVal)
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  
  ;; Values with False Contract
  (TCons K (TCons @ ι ⊥))
  (TAbs (λ x S) (TAbs @ ι ⊥))
  (TVal SVal (TVal @ ι ⊥))
  
  ;; Terms with Immediate Contracts/ False
  (TI SNonVal (TI @ ι I) (TI @ ι ⊥))
  
  ;; Terms with Delayed Contracts
  (TQ TVal TI (TQ @ ι Q))
  
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
   ((in-hole VCtx (λ x M)) (M @ ι Q))
   
   ;; Checkes of delayed contracts
   ((M @ ι Q) N) 
   
   ;; Imediate contracts on values
   ((in-hole VCtx K) @ ι I)
   ((in-hole VCtx (λ x M)) @ ι I)
   ;((λ x M) @ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ι C))
   
   ;; True
   (M @ ι ⊤)
   
   
   ;; Restructuring
   ;; -------------
   
   ;; Intersection betenn immediate and delayed contract
   (M @ ι (I ∩ C))
   
   ;; Union contracts
   (M @ ι (C ∪ D))
   
   ;; Nested delayed contracts
   ((M @ ι_0 Q) @ ι_1 I)
   ((M @ ι_0 Q) @ ι_1 ⊥)
   
   ;; Top-level assertions
   (M @ ♭ C))
  
  
  
  ;; Contexts
  ;; ========
  
  ;; Baseline Reduction Context
  ;; --------------------------
  ((F G H) hole (λ x F) (F M) (T F) (op T ... F M ...) (if T ... F M ...) (F @ b C))
  
  ;; Assertion Context
  ;; -----------------
  (VCtx hole (VCtx @ ι ⊥)))


#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Baseline-reduction
;; ==================
;; Verifies all (immediate) contracts that can be check at compile time
;; and unrolls all contracts.

(define Baseline-reduction
  (reduction-relation
   λCon-Baseline
   #:domain (ς any)
   
   ;; Constraint Generation
   ;; ---------------------
   ;; Rule [Unfold/Assert] creates a top-level constraint for contrat C.
   ;; Rules [Unfold/Union] and [Unfold/Intersection] unfods an 
   ;; union/intersection contract (all immediate).
   
   (--> (ς
         (in-hole F (T @ ♭ C)))
        (((♭ ◃ ι) ς)
         (in-hole F (T @ ι C)))
        "Unfold/Assert"
        (fresh ι))
   
   (--> (ς
         (in-hole F (T @ ι (C ∪ D)))) 
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F ((T @ ι1 C) @ ι2 D)))
        "Unfold/Union"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (T @ ι (I ∩ C)))) 
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F ((T @ ι1 I) @ ι2 C)))
        "Unfold/Intersection"
        (fresh ι1 ι2))
   
   ;; Unfold
   ;; ------
   ;; Rules [Unfold/D-Function] and [Unfold/D-Intersection] unfolds a 
   ;; function contract (delayed intersection contract).
   
   (--> (ς
         (in-hole F ((T_0 @ ι (C → D)) T_1)))
        (((ι ◃ (ι1 → ι2)) ς)
         (in-hole F ((T_0 (T_1 @ ι1 C)) @ ι2 D)))
        "Unfold/D-Function"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F ((T_0 @ ι (Q ∩ R)) T_1)))
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F (((T_0 @ ι1 Q) @ ι2 R) T_1)))
        "Unfold/D-Intersection"
        (fresh ι1 ι2))
   
   ;; Unroll
   ;; ------
   ;; Rule [Unroll] unrolles the contract of a contracted argument 
   ;; to all uses of the argument.
   
   (--> (ς
         (in-hole F ((in-hole VCtx (λ x S)) (T @ ι Q))))
        (ς
         (in-hole F ((λ x (unroll x Q ι S)) T)))
        "Unroll")
   
   ;; Lower (down)
   ;; ------------
   ;; Rule [Lower] creates a new function contarct from the 
   ;; contract of the function's body.
   
   (--> (ς
         (in-hole F (λ x (T @ ι C))))
        (ς
         (in-hole F ((λ x T) @ ι (⊤ → C))))
        "Lower")
   
   ;; Switch Order
   ;; ------------
   ;; Rule [Reverse/I] changes the order of contracts such that the delayed contrtact 
   ;; can be unrolled.
   
   (--> (ς
         (in-hole F ((T @ ι_0 Q) @ ι_1 I)))
        (ς
         (in-hole F ((T @ ι_1 I) @ ι_0 Q)))
        "Reverse/I")
   
   (--> (ς
         (in-hole F ((T @ ι_0 Q) @ ι_1 ⊥)))
        (ς
         (in-hole F ((T @ ι_1 ⊥) @ ι_0 Q)))
        "Reverse/False")
   
   ;; Valid Contracts
   ;; ---------------
   ;; Removes (term ⊤) contracts.
   
   (--> (ς
         (in-hole F (T @ ι ⊤)))
        (ς
         (in-hole F T))
        "Recude/True")
   
   ;; Predicate Verification
   ;; ----------------------
   ;; Evaluates predicates on values.
   
   (--> (ς
         (in-hole F ((in-hole VCtx V) @ ι predefined)))
        (ς
         (in-hole F ((in-hole VCtx V) @ ι (lookup predefined))))
        "Lookup")
   
   (--> (ς
         (in-hole F ((in-hole VCtx V) @ ι (flat M))))
        (ς
         (in-hole F ((in-hole VCtx V) @ ι ⊤)))
        "Verify/True"
        (where W (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (· (M V)))))))
        (side-condition (not (false? (term W)))))
   
   (--> (ς
         (in-hole F ((in-hole VCtx V) @ ι (flat M))))
        (ς
         (in-hole F ((in-hole VCtx V) @ ι ⊥)))
        "Verify/False"
        (where W (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (· (M V)))))))
        (side-condition (false? (term W))))
   
   ))

#|
 _   _              _ _ 
| | | |_ _  _ _ ___| | |
| |_| | ' \| '_/ _ \ | |
 \___/|_||_|_| \___/_|_|
                        
|#

;; Function unroll : x Q M -> N
;; ----------------------------
;; Unrolls a delayed contract Q of function x 
;; to all uses of x

(define-metafunction λCon-Baseline
  unroll : x Q b any -> any
  
  ;; Don't continue if x is bound λ's body
  [(unroll x Q b (λ x M)) (λ x M)]
  
  ;; Continue unrollong on λ's body
  [(unroll x Q b (λ y M)) (λ y (unroll x Q b M))]
  
  ;; Put contract to the usage of x
  [(unroll x Q b x) (x @ b Q)]
  
  ;; Continue unrollong on the structure of M
  [(unroll x Q b (any ...)) ((unroll x Q b any) ...)]
  
  ;; Return the target expression M if
  ;; none of the previous rules match
  [(unroll x Q b any) any])

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

;; Term of (⇓/Term)
;; ----------------
(define-metafunction λCon
  termOf : (ς T) -> T
  [(termOf (ς T)) T])

;; State of (⇓/State)
;; ------------------
(define-metafunction λCon
  stateOf : (ς T) -> ς
  [(stateOf (ς T)) ς])


;; Canonical? (non-reducable terms)
;; --------------------------------
(define canonical?
  (redex-match? λCon-Baseline T))

;; Reducible? (non-canonical terms)
;; --------------------------------
(define reducible? 
  (redex-match? λCon-Baseline Reducible))

;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Baseline~~> ς configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation Baseline-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Baseline~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Baseline-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))