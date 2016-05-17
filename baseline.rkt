#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

(require "symbolic.rkt")

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

(define-extended-language λCon-Baseline λCon-Symbolic)

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
  (extend-reduction-relation
   Symbolic-reduction
   λCon-Baseline
   #:domain (ς any)
   
   ;(define Baseline-reduction
   ;  (reduction-relation
   ;   λCon-Baseline
   ;   #:domain (ς any)
   
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
         ((in-hole F ((T_0 @ ι1 Q) T_1)) ∩∩ (in-hole F ((T_0 @ ι2 R) T_1))))
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
   
   (--> (ς
         (in-hole F (T @ ι ⊥)))
        (ς
         (in-hole F (blame ♭)))
        "Blame"
        (where (blame ♭) (blame-of ι ς)))
   
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