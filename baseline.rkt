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
   x (+blame ♭) (-blame ♭)
      (TI TQ) (TCons TQ) (TAbs TI) (TAbs TVal) (S S) ;;(TVal TVal) (TI TV) (KF T) (K TQ)
      (S TI)  ;(TVal S)
      ;(TVal TI)

      (op T ...) (if T_0 T_1 T_2))
  
  ;; Source Terms
  (S SVal SNonVal) ;(S @ ι ⊥) 
  
  
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  
  ;; TODO
  (TCons K (TCons @ ι ⊥))
  (TAbs (λ x S) (TAbs @ ι ⊥))
  
  ;; Values with False Contarcts
  (TVal SVal (TVal @ ι ⊥))
    
  ;; Terms with Immediate Contracts/ False
  (TI SNonVal (TI @ ι I) (TI @ ι ⊥))
  
  ;; Terms with Delayed Contracts
  (TQ TVal TI (TQ @ ι Q))
  
  ;; Canonical Terms (non-reducable terms)
  (T TQ (T_0 ∥ T_1))
  
  
  
  ;; Reducable terms (non-cannonical terms)
  ;; ======================================
  
  (Reducible
   
   ;; Terms containing a reducable term
   (λ x Reducible) (Reducible M) (M Reducible) (op M ... Reducible N ...) (if M ... Reducible N ...)   (Reducible @ b C)
   
   ;; Optimization
   ;; ------------
   
   ;; Delayed checkes of a delayed contract
   ;((λ x M) (M @ ι Q))
   ((in-hole ACtx (λ x M)) (M @ ι Q))
   
   ;; Checked of delayed contracts
   ((M @ ι Q) N) 
   
   ;; Imediate contracts in values
   ((in-hole ACtx K) @ ι I)
   ;(K @ ι I)
   ((in-hole ACtx (λ x M)) @ ι I)
   ;((λ x M) @ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ι C))
   
   ;; True
   (M @ ι ⊤)
   
   ;; False
   ;(M @ ι ⊥)
   
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
  ((F G H) hole (λ x F) (F M) (T F) (op T ... F M ...) (if T ... F M ...) (F @ b C) (F ∥ N) (T ∥ F))
  
  ;; Function Body Context
  ;; ---------------------
  ;; Reduction Context without abstraction.
  (BCtx hole (BCtx M) (T BCtx) (op T ... BCtx M ...) (BCtx @ b C))
  ;(BCtx ∥ T) (T ∥ BCtx)) ;; (if T ... BCtx T ...)
  
  ;; Assertion Context
  ;; -----------------
  ((ACtx CCtx) hole (ACtx @ ι C))
  (ICtx hole (ICtx @ ι I)) ;; TODO
  (QCtx hole (QCtx @ ι I)) ;; TODO
  
  ;; Parallel Observations
  (∥ ∩∩ ∪∪))

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
         ((in-hole F (T @ ι1 C)) ∪∪ (in-hole F (T @ ι2 D))))
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
         ;(in-hole F (((T_0 @ ι1 Q) @ ι2 R) T_1))) ;; TODO
         ((in-hole F ((T_0 @ ι1 Q) T_1)) ∩∩ (in-hole F ((T_0 @ ι2 R) T_1))))
        "Unfold/D-Intersection"
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
   
   ;; Lower (down)
   ;; ------------
   ;; Rule [Lower] creates a new function contarct from the 
   ;; contract of the function's body.
   
   ;; TODO, Lower only triggers when the valuation A[[S1]] @ C
   
   (--> (ς
         (in-hole F (λ x (T @ ι C))))
        (ς
         (in-hole F ((λ x T) @ ι (⊤ → C))))
        "Lower")
   
   ;; Switch Order
   ;; ------------
   ;; Rule [Switch] changes the order of contracts such that imemdiate contracts
   ;; can be checked right awar.
   
   ;; NOTE, not required because of assertion context
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
   
   ;; Blame
   ;; ---------------
   ;; Removes (term ⊥) contracts.
   
   ;; TODO: ⊥ mus remain
   ;; lift than reduces the whole context to blame
   
;   (--> (ς
;         (in-hole F (T @ ι ⊥)))
;        (ς
;         (in-hole F (blame ♭)))
;        "Reduce/False"
;        (where (blame ♭) (blame-of ι ς)))
   
   ;; Predicate Verification
   ;; ----------------------
   ;; Evaluates predicates on values.
   
   ;; NOT REQUIRED TO HABE AN ASSERTION CONTEXT HERE
   
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
   
   
   ;; Join Traces
   ;; -----------
   ;; Joins splitted observations.
   
   (--> (ς
         (in-hole F ((in-hole H (in-hole ACtx_l S_l))
                     ∥
                     (in-hole H (in-hole ACtx_r S_r)))))
        (ς
         (in-hole F ((in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_l S_r)))
                     ∥
                     (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_r S_l))))))
        "Join/Term"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole H (in-hole ACtx_l S_l))
                            ∥
                            (in-hole H (in-hole ACtx_r S_r))))))))
   
   (--> (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_l S_r))) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_r S_l))) T_r ... )))))
        "Join/Op"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole G (op T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                            ∥
                            (in-hole G (op T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... ))))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_l S_r))) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_r S_l))) T_r ... )))))
        "Join/If"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole G (if T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                            ∥
                            (in-hole G (if T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... ))))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (T ... (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_l S_r))) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (in-hole (⊔ ACtx_l ACtx_r) (√ ∥ S_r S_l))) T_r ... )))))
        "Join/App"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole G (T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                            ∥
                            (in-hole G (T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... ))))))))
   
   (--> (ς
         (in-hole F (T ∥ T))) 
        (ς
         (in-hole F T)) 
        "Join")
   
   ))




(define-metafunction λCon-Baseline
  ≈ : T T -> boolean
  ;; equals terms
  [(≈ T T) #t]
  ;; equal contracts on differnt terms
  [(≈ (T_l @ ι C) (T_r @ ι C)) #t]
  ;; one side blame
  [(≈ (blame ♭) T) #f]
  [(≈ T (blame ♭)) #f]
  ;; differnt terms without contract
  [(≈ S_l S_r) #t]
  ;; othweise
  [(≈ any ...) #f])

(define-metafunction λCon-Baseline
  ⊔ : ACtx ACtx -> ACtx
  [(⊔ ACtx_l ACtx_r) (in-hole ACtx_r ACtx_l)])

(define-metafunction λCon-Baseline
  √ : ∥ T T -> T
  ;; intersection/ negative blame
  [(√ ∩∩ (-blame ♭) T) T]
  [(√ ∩∩ T (-blame ♭)) T]
  ;; intersection/ positive blame
  [(√ ∩∩ (+blame ♭) T) (+blame ♭)]
  [(√ ∩∩ T (+blame ♭)) (+blame ♭)]
  ;; union/ negative blame
  [(√ ∪∪ (-blame ♭) T) (-blame ♭)]
  [(√ ∪∪ T (-blame ♭)) (-blame ♭)]
  ;; union/ positive blame
  [(√ ∪∪ (+blame ♭) T) T]
  [(√ ∪∪ T (+blame ♭)) T]
  
  ;; XXX
  [(√ ∩∩ T S) T]
  [(√ ∪∪ T S) T]) ;; TODO


;; TODO, use join with (∩∩ ♭)

#|
  (define-metafunction λCon-Baseline
    ⊔/x : T T -> T
    ;; intersection/ negative blame
    [(⊔/x ∩∩ (-blame ♭) T) T]
    [(⊔/x ∩∩ T (-blame ♭)) T]
    ;; intersection/ positive blame
    [(⊔ ∩∩ (+blame ♭) T) (+blame ♭)]
    [(⊔ ∩∩ T (+blame ♭)) (+blame ♭)]
    ;; union/ negative blame
    [(⊔ ∪∪ (-blame ♭) T) (-blame ♭)]
    [(⊔ ∪∪ T (-blame ♭)) (-blame ♭)]
    ;; union/ positive blame
    [(⊔ ∪∪ (+blame ♭) T) T]
    [(⊔ ∪∪ T (+blame ♭)) T])
 |# 









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
    _     _      
 _ | |___(_)_ _  
| || / _ \ | ' \ 
 \__/\___/_|_||_|
                 
|#
;; TODO, change join function.
;; A context/subject blame may only be removed if it is a blame term from the splitted contract.
;; Thus, say ∥ = (∪∪ ♭).

(define-metafunction λCon-Baseline
  join : ∥ M M -> M
  ;; intersection/ negative blame
  [(join ∩∩ (-blame ♭) T) T]
  [(join ∩∩ T (-blame ♭)) T]
  ;; intersection/ positive blame
  [(join ∩∩ (+blame ♭) T) (+blame ♭)]
  [(join ∩∩ T (+blame ♭)) (+blame ♭)]
  ;; union/ negative blame
  [(join ∪∪ (-blame ♭) T) (-blame ♭)]
  [(join ∪∪ T (-blame ♭)) (-blame ♭)]
  ;; union/ positive blame
  [(join ∪∪ (+blame ♭) T) T]
  [(join ∪∪ T (+blame ♭)) T])


#|
  ___      _         _      _         ___ _                
 / __|__ _| |__ _  _| |__ _| |_ ___  | _ ) |__ _ _ __  ___ 
| (__/ _` | / _| || | / _` |  _/ -_) | _ \ / _` | '  \/ -_)
 \___\__,_|_\__|\_,_|_\__,_|\__\___| |___/_\__,_|_|_|_\___|
                                                           
|#

;; root-of
;; -------
;; Calculates the root (♭) of blame indetifier b.
(define-metafunction λCon
  root-of : b ς -> ♭
  [(root-of ♭ ς) ♭]
  [(root-of ι ς) (root-of (parent-of ι ς) ς)])

;; parent-of
;; ---------
;; Calculates the parent blame indentifier b of blame variable ι.
(define-metafunction λCon
  parent-of : b ς -> b
  [(parent-of ι_0 ((b ◃ (ι_0 → ι_1)) ς)) b]
  [(parent-of ι_1 ((b ◃ (ι_0 → ι_1)) ς)) b]
  [(parent-of ι_0 ((b ◃ (ι_0 ∩ ι_1)) ς)) b]
  [(parent-of ι_1 ((b ◃ (ι_0 ∩ ι_1)) ς)) b]
  [(parent-of ι_0 ((b ◃ (ι_0 ∪ ι_1)) ς)) b]
  [(parent-of ι_1 ((b ◃ (ι_0 ∪ ι_1)) ς)) b]
  [(parent-of ι   ((b ◃ (¬ ι)) ς)) b]
  [(parent-of ι   ((b ◃ ι) ς)) b]
  [(parent-of ι   ()) ι]
  [(parent-of ι   ((b ◃ κ) ς)) (parent-of ι ς)])

;; invert
;; ------
;; Inverts a blame.
(define-metafunction λCon
  invert : blame -> blame
  [(invert +blame) -blame]
  [(invert -blame) +blame])

;; constraint-of
;; -------------
;; Looks for a constraint in state.
(define-metafunction λCon
  constraint-of : b ς -> κ
  [(constraint-of ι_0 ((b ◃ (ι_0 → ι_1)) ς)) (ι_0 → ι_1)]
  [(constraint-of ι_1 ((b ◃ (ι_0 → ι_1)) ς)) (ι_0 → ι_1)]
  [(constraint-of ι_0 ((b ◃ (ι_0 ∩ ι_1)) ς)) (ι_0 ∩ ι_1)]
  [(constraint-of ι_1 ((b ◃ (ι_0 ∩ ι_1)) ς)) (ι_0 ∩ ι_1)]
  [(constraint-of ι_0 ((b ◃ (ι_0 ∪ ι_1)) ς)) (ι_0 ∪ ι_1)]
  [(constraint-of ι_1 ((b ◃ (ι_0 ∪ ι_1)) ς)) (ι_0 ∪ ι_1)]
  [(constraint-of ι   ((b ◃ (¬ ι)) ς)) (¬ ι)]
  [(constraint-of ι   ((b ◃ ι) ς)) ι]
  [(constraint-of ι   ()) ι]
  ;; recursive lookup
  [(constraint-of ι   ((b ◃ κ) ς)) (constraint-of ι ς)])

;; sign-of
;; -------
;; Computes the sign a blame identifier.
(define-metafunction λCon
  sign-of : b ς -> blame
  [(sign-of ♭ ς) +blame]
  [(sign-of ι ς) (sign-in ι (constraint-of ι ς) ς)])

;; sign-in
;; -------
;; Computes the sign of a blame identifier in a constraint.
(define-metafunction λCon
  sign-in : b κ ς -> blame
  [(sign-in ι_0 (ι_0 → ι_1) ς) (invert (sign-of (parent-of ι_0 ς) ς))]
  [(sign-in ι   (¬ ι) ς)       (invert (sign-of (parent-of ι ς) ς))]
  ; otherwise
  [(sign-in ι κ ς)             (sign-of (parent-of ι ς) ς)])

;; sign-in
;; -------
;; Produces a balme term for a blame identifier.
(define-metafunction λCon
  blame-of : b ς -> (blame ♭)
  [(blame-of ι ς) ((sign-of ι ς) (root-of ι ς))])

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