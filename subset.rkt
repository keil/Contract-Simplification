#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

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

(define-extended-language λCon-Subset λCon-Baseline
  
  ;; Contexts
  ;; ========
  
  ;; Baseline Reduction Context
  ;; --------------------------
  ((F G H) .... (F ∥ N) (T ∥ F))
  
  ;; Function Body Context
  ;; ---------------------
  ;; Reduction Context without abstraction.
  (BCtx hole (T ... BCtx M ...) (op T ... BCtx M ...) (BCtx @ ♭ C) (BCtx @ ♭ ι C))
  
  ;; Assertion Context
  ;; -----------------
  (ACtx hole (ACtx @ ♭ ι C))
  
  
  ;; Canonical terms (λJ terms)
  ;; ==========================
  
  ;; Source Terms
  ;; ------------
  ;; Terms without a contract on the outermost position.
  
  ;; Values
  (SVal
   K (side-condition
      (name _fundec (λ x ... S))
      (not (redex-match? λCon-Subset (λ x ... y z ... (in-hole BCtx (y @ ♭ ι I))) (term _fundec)))))
  
  ;; Non-Values
  (SNonVal
   x (blame ♭) ;; TODO
   (TI TQ ...) (TCons TQ ...) (TAbs TI ...) (TAbs TVal ...)
   (op TQ ...) (if TQ_0 TQ_1 TQ_2))
  
  ;; Source Terms
  (S SVal SNonVal)
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  
  ;; Terms with Immediate Contracts/ False
  (TI SNonVal
      (SNonVal @ ♭ ι I)
      (side-condition 
       ((in-hole ACtx (TI @ ♭_i ι_i (name _I I))) @ ♭_j ι_j (name _J J))
       (not (or
             (term (⊑/naive _I _J))
             (term (⊑/naive _J _I))))))
  
  ;; Terms with Delayed Contracts
  (TQ SVal TI 
      (SVal @ ♭ ι Q) (TI @ ♭ ι Q)
      (side-condition 
       ((in-hole ACtx (TQ @ ♭_q ι_q (name _Q Q))) @ ♭_r ι_r (name _R R))
       (not (or
             (term (⊑/naive _Q _R))
             (term (⊑/naive _R _Q))
             (and (term (⊑/ordinary _Q _R)) (equal? (term ♭_q) (term ♭_r)))
             (and (term (⊑/ordinary _R _Q)) (equal? (term ♭_q) (term ♭_r)))))))
  
  ;; Canonical Terms (non-reducable terms)
  (T TQ (T_0 ∥ T_1) ((blame ♭) @ ♭ ι ⊥)) ;; TODO
  
  
  
  ;; Reducable terms (non-cannonical terms)
  ;; ======================================
  
  (Reducible
   
   ;; Terms containing a reducable term
   (λ x Reducible) (M ... Reducible N ...) (op M ... Reducible N ...) (if M ... Reducible N ...)   (Reducible @ ♭ ι C)
   
   ;; Subsets
   ;; -------
   (side-condition 
    ((in-hole ACtx (M @ ♭_c ι_c (name _c C))) @ ♭_d ι_d (name _d D))
    (or
     (term (⊑/naive _C _D))
     (term (⊑/naive _D _C))))
   
   (side-condition 
    ((in-hole ACtx (M @ ♭ ι_c (name _c C))) @ ♭ ι_d (name _d D))
    (or 
     (term (⊑/semnatic _C _D))
     (term (⊑/ordinary _D _C))))
   
   ;; Optimization
   ;; ------------
   
   ;; Delayed checkes of a delayed contract
   ((in-hole VCtx (λ x ... M)) L ... (M_q @ ♭ ι Q) N ...)
   
   ;; Checkes of delayed contracts
   ((M @ ♭ ι Q) N ...) 
   
   ;; Imediate contracts on values
   ((in-hole VCtx K) @ ♭ ι I)
   ((in-hole VCtx (λ x ... M)) @ ♭ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ♭ ι C))
   
   ;; True
   (M @ ♭ ι ⊤)
   
   ;; False
   (M @ ♭ ι ⊥)
   
   ;; Restructuring
   ;; -------------
   
   ;; Intersection betenn immediate and delayed contract
   (M @ ♭ ι (I ∩ C))
   
   ;; Union contracts
   (M @ ♭ ι (C ∪ D))
   
   ;; Nested delayed contracts
   ((M @ ♭_q ι_q Q) @ ♭_i ι_i I)
   ((M @ ♭_q ι_q Q) @ ♭_i ι_i ⊥)
   
   ;; Top-level assertions
   (M @ ♭ C)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Subset Reduction
;; ================

(define Subset-reduction
  (extend-reduction-relation
   Baseline-reduction
   λCon-Subset
   #:domain (ς any)
   
   ;; Unfold
   ;; ------
   ;; Rule [Unfold/Union] and [Unfold/D-Intersection] forks an 
   ;; union or intersection contract.
   
   (--> (ς
         (in-hole F (T @ ♭ ι (C ∪ D))))
        (((ι ◃ (ι1 ∪ ι2)) ς)
         ((in-hole F (T @ ♭ ι1 C)) ∥ (in-hole F (T @ ♭ ι2 D))))
        "Unfold/Union"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F ((T_0 @ ♭ ι (Q ∩ R)) T_1 ...)))
        (((ι ◃ (ι1 ∩ ι2)) ς)
         ((in-hole F ((T_0 @ ♭ ι1 Q) T_1 ...)) ∥ (in-hole F ((T_0 @ ♭ ι2 R) T_1 ...))))
        "Unfold/D-Intersection"
        (fresh ι1 ι2))
   
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (ς
         (in-hole F (λ x ... y z ...  (in-hole BCtx (y @ ♭ ι I)))))
        (((ι ◃ (¬ ι1)) ς)
         (in-hole F ((λ x ... y z ... (in-hole BCtx y)) @ ♭ ι1 (build (x ⊤) ... (y I) (z ⊤) ... ⊤))))
        "Lift"
        (fresh ι1))
   
   ;; Lower (down)
   ;; ------------
   ;; Rule [Lower] creates a new function contarct from the 
   ;; contract of the function's body.
   
   (--> (ς
         (in-hole F (λ x ... (T @ ♭ ι C))))
        (ς
         (in-hole F ((λ x ... T) @ ♭ ι (build (x ⊤) ... C))))
        "Lower"        
        (side-condition (canonical?/Subset (term (T @ ♭ ι C))))        
        (side-condition ; Do not lower argument contracts.
         (not (redex-match? λCon-Subset (λ x ... y z ... (in-hole BCtx (y @ ♭ ι I))) (term (λ x ... (T @ ♭ ι C)))))))
   
   ;; Blame
   ;; ---------------
   ;; Reduces False to blame.
   ;; Note: ⊥ mus remain in the source program.
   
   (--> (ς
         (in-hole F (λ x ... (in-hole BCtx (T @ ♭ ι ⊥)))))
        (ς
         (in-hole F (λ x ... ((blame ♭) @ ♭ ι ⊥))))
        "Blame"
        (side-condition (not (redex-match? λCon-Subset (blame ♭) (term T))))
        (where (blame ♭) (blame-of ι ς)))
   
   (--> (ς
         (in-hole F (if T (in-hole BCtx (T_m @ ♭ ι ⊥)) N)))
        (ς
         (in-hole F (if T ((blame ♭) @ ♭ ι ⊥) N)))
        (side-condition (not (redex-match? λCon-Subset (blame ♭) (term T_m))))
        "Blame/If/True")
   
   (--> (ς
         (in-hole F (if T T_m (in-hole BCtx (T_n @ ♭ ι ⊥)))))
        (ς
         (in-hole F (if T T_m ((blame ♭) @ ♭ ι ⊥))))
        (side-condition (not (redex-match? λCon-Subset (blame ♭) (term T_n))))
        "Blame/If/False")
   
   (--> (ς
         (in-hole F (if T ((blame ♭) @ ♭ ι ⊥) ((blame ♭) @ ♭ ι ⊥))))
        (ς
         (in-hole F ((blame ♭) @ ♭ ι ⊥)))
        "Blame/If")
   
   (--> (ς
         (T @ ♭ ι ⊥))
        (ς
         (blame ♭))
        "Blame/Global"
        (side-condition (not (redex-match? λCon-Subset (blame ♭) (term T))))
        (where (blame ♭) (blame-of ι ς)))
   
   ;; Subset 
   ;; ---------------
   ;; Removes contracts based on already checked contarcts.
   
   (--> (ς
         (in-hole F ((in-hole ACtx (T @ ♭_c ι_c C)) @ ♭_d ι_d D)))
        (ς
         (in-hole F (in-hole ACtx (T @ ♭_c ι_c C))))
        "Subset/Inner"
        (side-condition (term (⊑/naive C D))))
   
   (--> (ς
         (in-hole F ((in-hole ACtx (T @ ♭_c ι_c C)) @ ♭_d ι_d D)))
        (ς
         (in-hole F (in-hole ACtx (T @ ♭_d ι_d D))))
        "Subset/Outer"
        (side-condition (term (⊑/naive D C))))
   
   ;; Condense 
   ;; ---------------
   ;; Removes contracts based on already checked contarcts.
   
   (--> (ς
         (in-hole F (in-hole ACtx ((T @ ♭ ι_c (C_d → C_r)) @ ♭ ι_d (D_d → D_r)))))
        (((ι_c ◃ ι) ((ι_d ◃ ι) ς))
         (in-hole F (in-hole ACtx (T @ ♭ ι (D_d → C_r)))))
        "Condense/1"
        (side-condition (term (⊑/ordinary (C_d → C_r) (D_d → D_r))))
        (side-condition (equal? (term (blame-of ι_c ς)) (term (blame-of ι_d ς))))
        (fresh ι))
   
   (--> (ς
         (in-hole F (in-hole ACtx ((T @ ♭ ι_c (C_d → C_r)) @ ♭ ι_d (D_d → D_r)))))
        (((ι_c ◃ ι) ((ι_d ◃ ι) ς))
         (in-hole F (in-hole ACtx (T @ ♭ ι (C_d → D_r)))))
        "Condense/2"
        (side-condition (term (⊑/ordinary (D_d → D_r) (C_d → C_r))))
        (side-condition (equal? (term (blame-of ι_c ς)) (term (blame-of ι_d ς))))
        (fresh ι))
   
   ;; Simplification
   ;; --------------
   ;; Simplification reduces intersection and union contracts
   ;; at assertion time. 
   
   (--> (ς
         (in-hole F (T @ ♭ (C ∩ D))))
        (ς
         (in-hole F (T @ ♭ C)))
        "Simplify/Intersection/1"
        (side-condition (term (⊑/ordinary C D))))
   
   (--> (ς
         (in-hole F (T @ ♭ (C ∩ D))))
        (ς
         (in-hole F (T @ ♭ D)))
        "Simplify/Intersection/2"
        (side-condition (term (⊑/ordinary D C))))
   
   (--> (ς
         (in-hole F (T @ ♭ (C ∪ D))))
        (ς
         (in-hole F (T @ ♭ D)))
        "Simplify/Union/1"
        (side-condition (term (⊑/ordinary C D))))
   
   (--> (ς
         (in-hole F (T @ ♭ (C ∪ D))))
        (ς
         (in-hole F (T @ ♭ C)))
        "Simplify/Union/2"
        (side-condition (term (⊑/ordinary D C))))
   ))

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
  [(parent-of ι   ·) ι]
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
  [(constraint-of ι   ·) ι]
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
  ≡ : (λ x ... M) (λ x ... M) -> boolean
  [(≡ (λ x ... M) (λ x ... M)) #t]
  [(≡ (λ x ... M) (λ x ... N)) #f]
  [(≡ (λ x ... y_m z_m ... M) (λ x... y_n z_n ... N))
   (≤ (λ x ... y z_m ... (subst y_m y M)) (λ x... y z_n ... (subst y_n y N)))
   (where y ,(variable-not-in (term ((λ x ... y_m z_m ... M) (λ x... y z_n ... N))) (term y)))]
  ;; Otherwise
  [(≡ any ...) #f])

;; Term Subset (≤)
;; ---------------
;; This metafunction models subset relations of predicates. The subset relation of predicates needs to be defined manually (by the developer) as the semantical subset of predicates cannot be determines (e.g. positive? ≤ (x <= 0)). For others, a SAT solver could solve the relation.
;; ---------------
;; Returns true if the left term is subset or equals to the reight term, 
;; otherwise false.

(define-metafunction λCon
  ≤ : (λ x ... M) (λ x ... M) -> boolean
  
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
  ⊑/ordinary : C D -> boolean
  
  [(⊑/ordinary C ⊤) #t]
  [(⊑/ordinary ⊥ D) #f]
  
  [(⊑/ordinary C D) ,(and (term (⊑/context D C)) (term (⊑/subject C D)))]
  [(⊑/ordinary any ...) #f])

;; Naive Subsets of Contracts (⊑)
;; ==============================

;; A contract C is subset of contract D iff
;; C is more restrictive than D.

;; If C ⊑/naive D then \forall .
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
  ⊑/naive : C D -> boolean
  
  [(⊑/naive C ⊤) #t]
  [(⊑/naive ⊥ D) #f]
  
  [(⊑/naive C D) ,(and (term (⊑/context C D)) (term (⊑/subject C D)))])

;; Context Subset (⊑/context)
;; --------------------------

(define-metafunction λCon
  ⊑/context-n : (C ...) (D ...) -> boolean
  [(⊑/context-n (C_0 C_1 ...) (D_0 D_1 ...)) ,(and
                                               (term (⊑/context C_0 D_0))
                                               (term (⊑/context-n (C_1 ...) (D_1 ...))))]
  [(⊑/context-n () ()) #t])

(define-metafunction λCon
  ⊑/context : C D -> boolean
  
  ;; Immediate Contracts
  [(⊑/context I J) #t]
  
  [(⊑/context C ⊤) #t]
  [(⊑/context ⊤ D) #t]
  [(⊑/context ⊥ D) #t]
  [(⊑/context C ⊥) #t]
  
  ;; Abstraction
  [(⊑/context (Λ x ... C) (Λ x ... D)) (⊑/context C D)]
  
  ;; Function Contract
  [(⊑/context (C_0 ... → D_0) (C_1 ... → D_1)) ,(and
                                                 (term (⊑/subject-n (C_0 ...) (C_1 ...)))
                                                 (term (⊑/context D_0 D_1)))]
  
  ;; Dependent Contract
  [(⊑/context (x ... → A_0) (x ... → A_1)) (⊑/context A_0 A_1)]
  
  ;; Intersection Contract  
  [(⊑/context C (D_0 ∩ D_1)) ,(or (term (⊑/context C D_0)) (term (⊑/context C D_1)))]
  [(⊑/context (C_0 ∩ C_1) D) ,(and (term (⊑/context C_0 D)) (term (⊑/context C_1 D)))]
  
  ;; Union Contract
  [(⊑/context (C_0 ∪ C_1) D) ,(or (term (⊑/context C_0 D)) (term (⊑/context C_1 D)))]
  [(⊑/context C (D_0 ∪ D_1)) ,(and (term (⊑/context C D_0)) (term (⊑/context C D_1)))]
  
  ;; If not otherwise mentioned
  [(⊑/context any ...) #f])

;; Subject Subset (⊑/subject)
;; --------------------------

(define-metafunction λCon
  ⊑/subject-n : (C ...) (D ...) -> boolean
  [(⊑/subject-n (C_0 C_1 ...) (D_0 D_1 ...)) ,(and
                                               (term (⊑/subject C_0 D_0))
                                               (term (⊑/subject-n (C_1 ...) (D_1 ...))))]
  [(⊑/subject-n () ()) #t])

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
  [(⊑/subject (Λ x ... C) (Λ x ... D)) (⊑/subject C D)]
  
  ;; Function Contract
  [(⊑/subject (C_0 ... → D_0) (C_1 ... → D_1)) ,(and (term (⊑/context-n (C_0 ...) (C_1 ...)))
                                                     (term (⊑/subject D_0 D_1)))]
  
  ;; Dependent Contract
  [(⊑/subject (x ... → A_0) (x ... → A_1)) (⊑/subject A_0 A_1)]
  
  ;; Intersection Contract
  [(⊑/subject C (D_0 ∩ D_1)) ,(and (term (⊑/subject C D_0)) (term (⊑/subject C D_1)))]
  [(⊑/subject (C_0 ∩ C_1) D) ,(or (term (⊑/subject C_0 D)) (term (⊑/subject C_1 D)))]
  
  ;; Union Contract
  [(⊑/subject (C_0 ∪ C_1) D) ,(and (term (⊑/subject C_0 D)) (term (⊑/subject C_1 D)))]
  [(⊑/subject C (D_0 ∪ D_1)) ,(or (term (⊑/subject C D_0)) (term (⊑/subject C D_1)))]
  
  ;; If not otherwise mentioned
  [(⊑/subject any ...) #f])

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
(define canonical?/Subset
  (redex-match? λCon-Subset T))

;; Reducible? (non-canonical terms)
;; --------------------------------
(define reducible?/Subset
  (redex-match? λCon-Subset Reducible))

;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Subset~~> ς configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation Subset-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Subset~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Subset-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))