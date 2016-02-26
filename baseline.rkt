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
  
  ;; Immediate Contracts
  ;; -------------------
  ((I J) .... (I ∩ J))
  
  ;; Delayed Contracts
  ;; -----------------
  ((Q R) .... (C → ⊤) (⊤ → C))
  
  
  ;; TODO
  ((L M N) .... (M @ ι C))
  
  
  ;; Canonical terms (λJ terms)
  ;; ------------------------------
  
  ;; Source Terms (T)
  
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
  ((T B) S TI TQ)
  
  
  ;; Reducable terms (non-cannonical terms)
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
   (T @ ♭ C)
   
   )
  
  
  
  
  
  
  
  ;; Final Terms (top-level)
  (Final
   T (x @ ι C))
  
  
  
  ;; Baseline Reduction Context
  ;; --------------------------
  ((F G H) hole (λ x F) (F M) (B F) (op B ... F M ...) (if F M N) (if B_0 F N) (if B_0 B_1 F) (F @ b C)) 
  ;; use (T F) instead of (B F) beacuse the function contract eeds to be unrolled
  ;; (F M) (T F)
  
  
  ;; Miscellaneous
  ;; -------------
  
  ;; True-Contracts
  (True ⊤ (True → True) (x → (Λ x True)) (True ∩ True) (True ∪ True)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Pre-evaluation
;; ==============
;; Verifies all (immediate) contracts that can be check at compile time
;; and unroll all intersection/union contracts.

;; TODO
;; implement top-level blame rule

(define Pre-evaluation
  (reduction-relation
   λCon-Baseline
   #:domain (ς any)
   
   ;; Constraint Generation
   ;; --------------------- 
   
   (--> (ς
         (in-hole F (B @ ♭ C))) ; TODO, use M
        (((♭ ◃ ι) ς)
         (in-hole F (B @ ι C)))
        "Unfold/Assert"
        (fresh ι))
   
   (--> (ς
         (in-hole F (B @ ι (C ∪ D)))) ; shoudl this be B
        (((ι ◃ (ι1 ∪ ι2)) ς)
         (in-hole F ((B @ ι1 C) @ ι2 D)))
        "Unfold/Union"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (B @ ι (I ∩ Q)))) 
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F ((B @ ι1 I) @ ι2 C)))
        "Unfold/Intersection"
        (fresh ι1 ι2))
   
   ;; Valid Contracts
   ;; ---------------
   
   (--> (ς
         (in-hole F (B @ ι True)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F B))
        "Recude/True")

      (--> (ς
         (in-hole F (B @ ι ⊥))) ;; Introduce false set
        (((ι ◃ (τ #f)) ς)
         (in-hole F B))
        "Recude/Flase")
   
   ;; Predicaste Verification
   ;; -----------------------
   
   (--> (ς
         (in-hole F (V @ ι predefined)))
        (ς
         (in-hole F (V @ ι (lookup predefined))))
        "Lookup")
   
   
   (--> (ς
         (in-hole F (V @ ι (flat M))))
        (ς
         (in-hole F (V @ ι ⊤)))
        "Verify/True"
        (where W (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (· (M V)))))))
        (side-condition (not (false? (term W)))))
   
   
   (--> (ς
         (in-hole F (V @ ι (flat M))))
        (ς
         (in-hole F (V @ ι ⊥)))
        "Verify/False"
        (where W (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (· (M V)))))))
        (side-condition (false? (term W))))
   ))


;; Subset Reduction
;; ================

;; or callit predicate redution

;; is it possible to reuse teh same context ?

(define Subset-reduction
  (extend-reduction-relation
   Pre-evaluation
   λCon-Baseline
   #:domain (ς any)
   
   (--> (ς
         (in-hole F ((B @ ι_0 I) @ ι_1 Q)))
        (ς
         (in-hole F ((B @ ι_1 Q) @ ι_0 I)))
        "Switch")
   
   ;   (--> (ς
   ;         (in-hole F ((B @ ι_0 C) @ ι_1 D)))
   ;        (ς
   ;         (in-hole F ((B @ (ι_0 ι_1) C) @ ι_1 (\\ D C))))
   ;        "Subset"
   ;        (side-condition (not (λCon-value? (term B)))))
   
   ))

;; reduction when in pos in pos cap even





;; Baseline Reduction
;; ------------------
;; Verifies all (immediate) contracts 
;; that can be check at compile time

;; TODO
;; implement top-level blame rule


;; Contract Propagation



(define Baseline-reduction
  (extend-reduction-relation
   Subset-reduction
   λCon-Baseline
   #:domain (ς M)
   
   ;; Unroll
   ;; ------
   ;; Rule [Unroll] unrolles the contract of a contracted argument 
   ;; to all uses of the argument.
   
   (--> (ς
         (in-hole F ((λ x S) (B @ ι Q))))
        (ς
         (in-hole F ((λ x (unroll x Q ι S)) B)))
        "Unroll")
   
   
   ;; Unfold
   ;; ------
   ;; Rule [Unfold] unfolds a function contract (intersection contract).
   
   (--> (ς
         (in-hole F ((B_0 @ ι (C → D)) B_1)))
        (((ι ◃ (ι1 → ι2)) ς)
         (in-hole F ((B_0 (B_1 @ ι1 C)) @ ι2 D)))
        "Unfold/Function"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F ((B_0 @ ι (Q ∩ R)) B_1)))
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F (((B_0 @ ι1 Q) @ ι2 R) B_1)))
        "Unfold/Intersection"
        (fresh ι1 ι2))
   
   ;; Lower (down)
   ;; ------------
   ;; Rule [Lower] creates a new function cntract from the 
   ;; contract of the function's body
   
   (--> (ς
         (in-hole F (λ x (B @ ι C))))
        (ς
         (in-hole F ((λ x B) @ ι (⊤ → C))))
        "Lower")
   
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (ς
         (in-hole F (λ x (in-hole H (x @ ι I))))) ;; ? all contracts? ;; use special context
        ;(((ι ◃ (ι1 ∩ ι2)) ς)
        (ς
         (in-hole F ((λ x (in-hole H x)) @ ι (I → ⊤))))
        ; (in-hole F ((λ x (in-hole F (x @ ι2 ⊤))) @ ι1 (I → ⊤))))
        "Lift"
        (fresh ι1 ι2))
   
   ;; Collapse
   ;; --------
   
   ;; TODO, what kind of contracts should be collapsed?
   ;; For example:
   ;; - Flat contarcts: (flat M) • (flat N) --> (flat (M • N))
   ;; - Function contracts: (⊤ → Num) • (Num → ⊤) --> (Num → Num)
   ;; - Function contracts: (⊤ → Num) • (⊤ → Pos) --> (⊤ → (Num • Pos))
   
   ; Collapse
   ;(--> (in-hole H ((S @ C) @ D))
   ;     (in-hole H (S @ (collapse C D)))
   ;     "Collaps"
   ;)
   ;(--> (ς
   ;      (in-hole F ((S @ Q) @ R)))
   ;     (ς
   ;      (in-hole F (S @ (collapse Q R))))
   ;     "Collaps/Delayed")
   
   ;(--> (ς
   ;      (in-hole F ((S @ I) @ J)))
   ;     (ς
   ;      (in-hole F (S @ (collapse I J))))
   ;     "Collaps/Immediate")
   
   ;; Swap
   ;; -------
   ;; Rule [Swap] moves delayed contracts to the outer possition.
   ;; This enables to unfold the contract on the outer position.
   ;; Hint: This rules changes the blame-semantcs 
   
   (--> (ς
         (in-hole F ((M @ ι_0 Q) @ ι_1 I)))
        (ς
         (in-hole F ((M @ ι_1 I) @ ι_0 Q)))
        "Swap"
        (side-condition (not (λCon-value? (term (M @ ι_0 Q))))))
   
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
 ___                     _   _         _ 
/ __| ___ _ __  __ _ _ _| |_(_)__ __ _| |
\__ \/ -_) '  \/ _` | ' \  _| / _/ _` | |
|___/\___|_|_|_\__,_|_||_\__|_\__\__,_|_|
                                         
  ___         _        _                    _   
 / __|___ _ _| |_ __ _(_)_ _  _ __  ___ _ _| |_ 
| (__/ _ \ ' \  _/ _` | | ' \| '  \/ -_) ' \  _|
 \___\___/_||_\__\__,_|_|_||_|_|_|_\___|_||_\__|
                                                
|#

;; Term Equivalence (≡)
;; --------------------
;; Returns true if both terms are identical under α conversion, 
;; otherwise false.

(define-metafunction λCon
  ≡ : (λ x M) (λ x M) -> boolean
  [(≡ (λ x M) (λ x M)) #t]
  [(≡ (λ x M) (λ x N)) #f]
  [(≡ (λ x M) (λ y N)) (≤ (λ z (subst x z M)) (λ z (subst y z N)))
                       (where z ,(variable-not-in (term ((λ x M) (λ y N))) (term z)))]
  ;; Otherwise
  [(≡ any ...) #f])

;; Term Subset (≤)
;; ---------------
;; This metafunction models subset relations of predicates. The subset relation of predicates needs to be defined manually (by the developer) as the semantical subset of predicates cannot be determines (e.g. positive? ≤ (x <= 0)). For others, a SAT solver could solve the relation.
;; ---------------
;; Returns true if the left term is subset or equals to the reight term, 
;; otherwise false.

(define-metafunction λCon
  ≤ : (λ x M) (λ x M) -> boolean
  
  ;; complex? ≤ number?
  [(≤ (λ x (complex? x))  (λ x (number? x)))   #t]
  
  ;; real? ≤ number?
  [(≤ (λ x (real? x))     (λ x (number? x)))   #t]
  
  ;; rational? ≤ real? ≤ number?
  [(≤ (λ x (rational? x)) (λ x (real? x)))     #t]
  [(≤ (λ x (rational? x)) (λ x (number? x)))   #t]
  
  ;; integer? ≤ rational? ≤ real? ≤ number?
  [(≤ (λ x (integer? x))  (λ x (rational? x))) #t]
  [(≤ (λ x (integer? x))  (λ x (real? x)))     #t]
  [(≤ (λ x (integer? x))  (λ x (number? x)))   #t]
  
  ;; (x >= 0) ≤ real? ≤ number?
  [(≤ (λ x (>= x 0)) (λ x (number? x)))        #t]
  [(≤ (λ x (>= x 0)) (λ x (real? x)))          #t]
  
  ;; positive? ≤ (x >= 0) ≤ real? ≤ number?
  [(≤ (λ x (positive? x)) (λ x (>= x 0)))      #t]
  [(≤ (λ x (positive? x)) (λ x (number? x)))   #t]
  [(≤ (λ x (positive? x)) (λ x (real? x)))     #t]
  
  ;; negative? ≤ real? ≤ number?
  [(≤ (λ x (negative? x)) (λ x (number? x)))   #t]
  [(≤ (λ x (negative? x)) (λ x (real? x)))     #t]
  
  ;; zero? ≤ number?
  [(≤ (λ x (zero? x)) (λ x (number? x)))       #t]
  
  ;; exact? ≤ number?
  [(≤ (λ x (exact x)) (λ x (number? x)))       #t]
  
  ;; inexact ≤ number?
  [(≤ (λ x (inexact x)) (λ x (number? x)))     #t]
  
  ;; even? ≤ integer? ≤ rational? ≤ real? ≤ number?
  [(≤ (λ x (even? x))  (λ x (integer? x)))     #t]
  [(≤ (λ x (even? x))  (λ x (rational? x)))    #t]
  [(≤ (λ x (even? x))  (λ x (real? x)))        #t]
  [(≤ (λ x (even? x))  (λ x (number? x)))      #t]
  
  ;; odd? ≤ integer? ≤ rational? ≤ real? ≤ number?
  [(≤ (λ x (odd? x))  (λ x (integer? x)))      #t]
  [(≤ (λ x (odd? x))  (λ x (rational? x)))     #t]
  [(≤ (λ x (odd? x))  (λ x (real? x)))         #t]
  [(≤ (λ x (odd? x))  (λ x (number? x)))       #t]
  
  ;; Otherwise
  [(≤ any ...) (≡ any ...)])





;; Semantics Subsets of Contracts (⊑)
;; ==================================

(define-metafunction λCon
  ⊑/semantic : C D -> boolean
  
  [(⊑/semantic C ⊤) #t]
  [(⊑/semantic ⊥ D) #f]
  
  [(⊑/semantic C D) ,(and (term (⊑/context D C)) (term (⊑/subject C D)))]
  [(⊑/semantic any ...) #f])



;; Naive Subsets of Contracts (⊑)
;; ==============================

;; A contract C is subset of contract D iff
;; C is more restrictive than D.

;; If C ⊑ D then \forall .
;; * V \in [[C]]+ => V \in [[D]]+
;; * E \in [[C]]- => E \in [[D]]-
;; resp.
;; * V \not\in [[D]]+ => V \not\in [[C]]+
;; * E \not\in [[D]]- => E \not\in [[C]]-

;; It follows that:
;;    E[[ M @ D ]] --> +blame/-blame
;; => E[[ M @ C ]] --> +blame/-blame
;; resp.
;;    E[[ M @ C ]] --> V
;; => E[[ M @ D ]] --> V

(define-metafunction λCon
  ⊑ : C D -> boolean
  
  [(⊑ C ⊤) #t]
  [(⊑ ⊥ D) #f]
  
  [(⊑ C D) ,(and (term (⊑/context C D)) (term (⊑/subject C D)))]
  [(⊑ any ...) #f])

(define-metafunction λCon
  ⊑/context : C D -> boolean
  
  ;; Immediate Contracts
  [(⊑/context I J) #t]
  
  ;; Abstraction
  [(⊑/context (Λ x C) (Λ x D)) (⊑/context C D)]
  
  ;; Function Contract
  [(⊑/context (C_0 → D_0) (C_1 → D_1)) ,(and (term (⊑/subject C_0 C_1)) (term (⊑/context D_0 D_1)))]
  
  ;; Dependent Contract
  [(⊑/context (x → A_0) (x → A_1)) (⊑/context A_0 A_1)]
  
  ;; Intersection Contract  
  [(⊑/context C (D_0 ∩ D_1)) ,(or (term (⊑/context C D_0)) (term (⊑/context C D_1)))]
  [(⊑/context (C_0 ∩ C_1) D) ,(and (term (⊑/context C_0 D)) (term (⊑/context C_1 D)))]
  
  ;; Union Contract
  [(⊑/context (C_0 ∪ C_1) D) ,(or (term (⊑/context C_0 D)) (term (⊑/context C_1 D)))]
  [(⊑/context C (D_0 ∪ D_1)) ,(and (term (⊑/context C D_0)) (term (⊑/context C D_1)))]
  
  ;; If not otherwise mentioned
  [(⊑/context any ...) #f])


(define-metafunction λCon
  ⊑/subject : C D -> boolean
  
  [(⊑/subject C ⊤) #t]
  [(⊑/subject ⊥ D) #t]
  
  ;; Predefined Contracts
  [(⊑/subject predefined D) (⊑/subject (lookup predefined) D)]
  [(⊑/subject C predefined) (⊑/subject C (lookup predefined))]
  
  ;; Flat Contracts
  [(⊑/subject (flat M) (flat N)) (≤ M N)]
  
  ;; Abstraction
  [(⊑/subject (Λ x C) (Λ x D)) (⊑/subject C D)]
  
  ;; Function Contract
  [(⊑/subject (C_0 → D_0) (C_1 → D_1)) ,(and (term (⊑/context C_0 C_1)) (implies (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 D_1))))]
  
  ;; Dependent Contract
  [(⊑/subject (x → A_0) (x → A_1)) (⊑/subject A_0 A_1)]
  
  ;; Intersection Contract
  [(⊑/subject C (D_0 ∩ D_1)) ,(and (term (⊑/subject C D_0)) (term (⊑/subject C D_1)))]
  [(⊑/subject (C_0 ∩ C_1) D) ,(or (term (⊑/subject C_0 D)) (term (⊑/subject C_1 D)))]
  
  ;; Union Contract
  [(⊑/subject (C_0 ∪ C_1) D) ,(and (term (⊑/subject C_0 D)) (term (⊑/subject C_1 D)))]
  [(⊑/subject C (D_0 ∪ D_1)) ,(or (term (⊑/subject C D_0)) (term (⊑/subject C D_1)))]
  
  ;; If not otherwise mentioned
  [(⊑/subject any ...) #f])






#|
  ___         _               _   
 / __|___ _ _| |_ _ _ __ _ __| |_ 
| (__/ _ \ ' \  _| '_/ _` / _|  _|
 \___\___/_||_\__|_| \__,_\__|\__|
                                  
 ___      _       _               _   _          
/ __|_  _| |__ __| |_ _ _ __ _ __| |_(_)___ _ _  
\__ \ || | '_ (_-<  _| '_/ _` / _|  _| / _ \ ' \ 
|___/\_,_|_.__/__/\__|_| \__,_\__|\__|_\___/_||_|
                                                 
|#


;; Contract Minus (I \ J)


;; Predicate Containment (∈)
;; ------------------------



;; Contract Difference (\\)
;; ------------------------

(define-metafunction λCon-Baseline
  \\ : C D -> C
  
  ;; ⊤ \ J 
  [(\\ ⊤ D) ⊤]
  ;; I \ J (e.g. Numer \ Positive) 
  [(\\ C D) ⊤ (side-condition (term (⊑ D C)))]
  
  ;; Right-Intersection
  [(\\ C (D_0 ∩ D_1)) (≈/ ((\\ C D_0) ∪ (\\ C D_1)))]
  ;; Right-Union
  [(\\ C (D_0 ∪ D_1)) (≈/ ((\\ C D_0) ∩ (\\ C D_1)))]
  
  ;; Left-Intersection
  [(\\ (C_0 ∩ C_1) D) (≈/ ((\\ C_0 D) ∩ (\\ C_1 D)))]
  ;; Left-Union
  [(\\ (C_0 ∪ C_1) D) (≈/ ((\\ C_0 D) ∪ (\\ C_1 D)))]
  
  ;; Otherwise
  [(\\ C D) C])


;; Contract Normalization (≈)
;; --------------------------

(define-metafunction λCon-Baseline
  ≈/ : C -> C
  [(≈/ (C ∩ D)) (≈ ((≈/ C) ∩ (≈/ D)))]
  [(≈/ (C ∪ D)) (≈ ((≈/ C) ∪ (≈/ D)))]
  [(≈/ (C → D)) (≈ ((≈/ C) → (≈/ D)))]
  [(≈/ any) (≈ any)])

(define-metafunction λCon-Baseline
  ≈ : C -> C
  
  [(≈ (I ∩ ⊥)) ⊥]
  [(≈ (⊥ ∩ J)) ⊥]
  [(≈ (I ∩ ⊤)) I]
  [(≈ (⊤ ∩ J)) J]
  
  [(≈ (I ∪ ⊥)) I]
  [(≈ (⊥ ∪ J)) J]
  [(≈ (I ∪ ⊤)) ⊤]
  [(≈ (⊤ ∪ J)) ⊤]
  
  [(≈ (C ∩ ⊥)) ⊥]
  [(≈ (⊥ ∩ D)) ⊥]
  
  [(≈ (C ∪ ⊥)) C]
  [(≈ (⊥ ∪ D)) D]
  
  
  [(≈ (C ∩ D)) C (side-condition (term (⊑/semantic C D)))]
  [(≈ (C ∩ D)) D (side-condition (term (⊑/semantic D C)))]
  
  [(≈ (C ∪ D)) D (side-condition (term (⊑/semantic C D)))]
  [(≈ (C ∪ D)) C (side-condition (term (⊑/semantic D C)))]
  
  [(≈ C) C])











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
  (redex-match λCon-Baseline T))

;; Reducible? (non-canonical terms)
;; --------------------------------
(define reducible? 
  (redex-match λCon-Baseline Reducible))

;; Final? (top-level final terms)
;; ------------------------------
(define final? 
  (redex-match λCon-Baseline Final))



;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon~~> ς M)
  (if (redex-match? λCon M M)
      (car (apply-reduction-relation Baseline-reduction (term (,ς ,M))))
      (error "Invalid λCon-term:" M)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Baseline-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))