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
  
  ;; Immediate Contracts
  ;; -------------------
  ((I J) .... (I ∩ J))
  
  ;; Delayed Contracts
  ;; -----------------
  ((Q R) .... (C → ⊤) (⊤ → C))
  
  ;; Terms
  ;; -----
  ((L M N) .... (M @ ι C) (M || N))
  
  
  
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
  (TI S1 (TI @ ι I) (TI @ ι ⊥))
  
  ;; Terms with Delayed Contracts
  (TQ S TI (TQ @ ι Q))
  
  ;; Canonical Terms (non-reducable terms)
  (T TQ (S @ ι ⊥))
  
  ;; TODO
  ;; As every T @ ⊥ is more restrictive than every other contract
  ;; we only have T @ ⊥ once, because ervy other contract is removed
  
  
  ;; Reducable terms (non-cannonical terms)
  ;; ======================================
  
  ;;  terms ()
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
   (K @ ι I) #| (x @ ι I) |# ((λ x M) @ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ι C))
   
   ;; True
   (M @ b ⊤)
   
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
  
  
  
  ;; Contexts
  ;; ========
  
  ;; Baseline Reduction Context
  ;; --------------------------
  ((F G H) hole (λ x F) (F M) (T F) (op T ... F M ...) (if T ... F M ...) (F @ b C)
           (F || N) (T || F))
  
  ;; Function Body Context
  ;; ---------------------
  ;; Reduction Context without abstraction.
  (BCtx hole (BCtx M) (T BCtx) (op T ... BCtx M ...) (if T ... BCtx M ...) (BCtx @ b C)
        (BCtx || N) (T || BCtx))
  
  ;; Assertion Context
  ;; -----------------
  (ACtx hole (ACtx @ ι C)) ;; TODO (X @ ι C) ?
  
  ;; Miscellaneous
  ;; =============
  
  ;; True-Contracts
  ;(True ⊤ (True → True) (x → (Λ x True)) (True ∩ True) (True ∪ True))
  
  ;; False-Contracts
  ;(False ⊥)
  )

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
        (((ι ◃ (ι1 ∪ ι2)) ς)
         ;(in-hole F ((T @ ι1 C) @ ι2 D))) ;; TODO
         ((in-hole F (T @ ι1 C)) || (in-hole F (T @ ι2 D))))
        "Unfold/Union"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (T @ ι (I ∩ Q)))) 
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F ((T @ ι1 I) @ ι2 C)))
        "Unfold/Intersection"
        (fresh ι1 ι2))
   
   ;; Unroll
   ;; ------
   ;; Rule [Unroll] unrolles the contract of a contracted argument 
   ;; to all uses of the argument.
   
   (--> (ς
         (in-hole F ((λ x S) (T @ ι Q))))
        (ς
         (in-hole F ((λ x (unroll x Q ι S)) T)))
        "Unroll")
   
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
         ;(in-hole F (((T_0 @ ι1 Q) @ ι2 R) T_1))) ;; TODO
         ((in-hole F ((T_0 @ ι1 Q) T_1)) || (in-hole F ((T_0 @ ι2 R) T_1))))
        "Unfold/D-Intersection"
        (fresh ι1 ι2))
   
   
   
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
   ;; Rule [Switch] changes the order of contracts such that imemdiate contracts
   ;; can be checked right awar.
   
   ;; TODO
   ;(--> (ς
   ;      (in-hole F ((T @ ι_0 I) @ ι_1 Q)))
   ;     (ς
   ;      (in-hole F ((T @ ι_1 Q) @ ι_0 I)))
   ;     "Switch")
   
   
   
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
         (in-hole F ((in-hole ACtx V) @ ι predefined)))
        (ς
         (in-hole F ((in-hole ACtx V) @ ι (lookup predefined))))
        "Lookup")
   
   (--> (ς
         (in-hole F ((in-hole ACtx V) @ ι (flat M))))
        (ς
         (in-hole F ((in-hole ACtx V) @ ι ⊤)))
        "Verify/True"
        (where W (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (· (M V)))))))
        (side-condition (not (false? (term W)))))
   
   (--> (ς
         (in-hole F ((in-hole ACtx V) @ ι (flat M))))
        (ς
         (in-hole F ((in-hole ACtx V) @ ι ⊥)))
        "Verify/False"
        (where W (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (· (M V)))))))
        (side-condition (false? (term W))))
   
   
   
   ;; Join
   ;; ----
   ;; Merges the splitted evaluations.
   
   (--> (
         (in-hole F ((in-hole H (T @ ι C)) || (in-hole H S))))
        (ς
         (in-hole F ((in-hole H (T @ ι C)) || (in-hole H (S @ ι C)))))
        "Join/LeftContract"
        (side-condition (and (canonical? (term (in-hole H (T @ ι C))))
                             (canonical? (term (in-hole H S)))))
        )
   
   (--> (ς
         (in-hole F ((in-hole H S) || (in-hole H (T @ ι C)))))
        (ς
         (in-hole F ((in-hole H (S @ ι C)) || (in-hole H (T @ ι C)))))
        "Join/RightContract"
        
        (side-condition (and (canonical? (term (in-hole H S)))
                             (canonical? (term (in-hole H (T @ ι C))))))
        ) 
   
   (--> (ς
         (in-hole F ((in-hole H (T_1 @ ι_1 C)) || (in-hole H (T_2 @ ι_2 D)))))
        (ς
         (in-hole F ((in-hole H ((T_1 @ ι_1 C) @ ι_2 D)) || (in-hole H ((T_2 @ ι_1 C) @ ι_2 D)))))
        "Join/LeftRightContract"
        
        (side-condition (and (canonical? (term (in-hole H (T_1 @ ι_1 C))))
                             (canonical? (term (in-hole H (T_2 @ ι_2 D))))
                             (not (eq? (term C) (term D)))
                             ))
        )
   
   
   (--> (ς
         (in-hole F ((in-hole H (T_1 (T_11 @ ι_1 C))) || (in-hole H (T_2 (T_22 @ ι_2 D))))))
        (ς
         (in-hole F ((in-hole H (T_1 ((T_11 @ ι_1 C) @ ι_2 D))) || (in-hole H (T_2 ((T_22 @ ι_1 C) @ ι_2 D))))))
        "Join/App"
        
        (side-condition (and (canonical? (term (in-hole H (T_1 (T_11 @ ι_1 C)))))
                             (canonical? (term (in-hole H (T_2 (T_22 @ ι_2 D)))))
                             (not (eq? (term C) (term D)))
                             ))
        )
      
   (--> (ς
         (in-hole F (T || T))) 
        (ς
         (in-hole F T)) 
        "Join")

   ))
  
  
  
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
  
  ;; Canonical? (non-reducable terms)
  ;; --------------------------------
  (define canonical?
    (redex-match? λCon-Baseline T))
  
  ;; Reducible? (non-canonical terms)
  ;; --------------------------------
  (define reducible? 
    (redex-match? λCon-Baseline Reducible))
  
  ;; Final? (top-level final terms)
  ;; ------------------------------
  (define final? 
    (redex-match? λCon-Baseline Final))
  
  ;; λCon Reduction (λCon-->)
  ;; ------------------------
  (define
    (λCon/Baseline~~> ς M)
    (if (redex-match? λCon M M)
        (car (apply-reduction-relation Baseline-reduction (term (,ς ,M))))
        (error "Invalid λCon-term:" M)))
  
  ;; λCon Reduction (λCon-->*)
  ;; -------------------------
  (define
    (λCon/Baseline~~>* configuration)
    (if (redex-match? λCon (ς M) configuration)
        (car (apply-reduction-relation* Baseline-reduction (term ,configuration)))
        (error "Invalid λCon-term:" configuration)))