#lang racket
(require redex)

(require "lj.rkt")

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

(define-extended-language λCon λJ
  
  ;; Predicates
  (P ⊤ (M ⇒ P))
  
  ;; Contracts
  ;; ---------
  
  ;; False Values/Constants
  (false #f 0 "")
  
  ;; Contracts
  ((C D) I Q (C ∪ D) (I ∩ C))
  
  ; Immediate Contracts
  ((I J) (flat M) named (flat P ...))
  
  ; Delayed Contracts
  ((Q R) (C → D) (x → C) (Q ∩ R))
  
  
  
  ;; λCon Extention
  ;; --------------
  
  ;; Blame
  (blame +blame -blame)
  
  ;; Values
  ((U V W) .... ((λ x M) @ Q) blame)
  
  ;; Terms
  ((L M N) .... (M @ C) blame)
  
  ;; Contexts
  ((E F) .... (E @ C) (V @ (eval E)))
  
  
  
  ;; Blame Constraints
  ;; -----------------
  
  ;; blame labels
  (♭ (variable-prefix ♭))
  
  ;; blame variales
  (ι (variable-prefix ♭))
  
  ;; blame identifiers
  (b ♭ ι)
  
  ;; boolean values
  (B #t #f)
  
  ;; Constraints
  (κ B ι (ι_1 → ι_2) (ι_1 ∩ ι_2) (ι_1 ∪ ι_2))
  
  ;; State
  (ς · ((b ◃ κ) ς))
  
  ;; Solution (context/ subject)
  (ω (B_0 ∘ B_1))
  
  
  
  ;; Predefined Predicates
  ;; ---------------------
  
  ;; TODO, only for testing
  (named
   Any? ⊤
   None? ⊥
   Num?
   Str?
   Bool?
   Pos?
   Neg?
   Nat?
   )
  )

;; Predicate Subset
;; ================
;(< #t #f) defined on real
;(number? +nan.0)
;(real? +nan.0)

(define-metafunction λCon
  ≤ : P P -> boolean
  
  [(≤ P ⊤) #t]
  [(≤ P P) #t]
  
  [(≤ P (M ⇒ P)) #t]
  
  
  [(≤ P any) #f])

  ;; Predicates
  ;(P ⊤ (M ⇒ P))


(define-metafunction λCon
  ∈ : P (flat P ...) -> boolean
  [(∈ P_i (flat P_0 ... P_i P_i+1 ...)) #t] ;; subset
  [(∈ P any) #f])

(define-metafunction λCon
  ∈x : P (flat P ...) -> boolean
  [(∈x P_i (flat P_0 ... P_i P_i+1 ...)) #t]
  [(∈x P any) #f])




;; Naive Subsets of Contracts
;; ==========================

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
  [(⊑ C D) ,(and (term (⊑/context C D)) (term (⊑/subject C D)))])

(define-metafunction λCon
  ⊑/context : C D -> boolean
  ;; Immediate Contracts
  [(⊑/context I J) #t]  
  ;; Function Contract
  [(⊑/context (C_0 → D_0) (C_1 → D_1)) ,(and (term (⊑/subject C_0 C_1)) (term (⊑/context D_0 D_1)))]
  ;; Intersection Contract
  [(⊑/context (C_0 ∩ D_0) C_1) ,(and (term (⊑/context C_0 C_1)) (term (⊑/context D_0 C_1)))]
  [(⊑/context C_0 (C_1 ∩ D_1)) ,(or (term (⊑/context C_0 C_1)) (term (⊑/context C_0 D_1)))]
  ;; Union Contract
  [(⊑/context (C_0 ∪ D_0) C_1) ,(or (term (⊑/context C_0 C_1)) (term (⊑/context D_0 C_1)))]
  [(⊑/context C_0 (C_1 ∪ D_1)) ,(and (term (⊑/context C_0 C_1)) (term (⊑/context C_0 D_1)))]
  ;; Dependent
  ;; TODO
  )

(define-metafunction λCon
  ⊑/subject : C D -> boolean
  ;; Flat Contracts
  [(⊑/subject (flat M) (flat M)) #t]
  [(⊑/subject (flat M) (flat N)) #f]
  ;; Predefined Contracts
  ;; TODO
  [(⊑/subject named ⊤) #t]
  [(⊑/subject ⊥ named) #t]
  [(⊑/subject named named) #t]
  [(⊑/subject named named) #t]
  [(⊑/subject Nat? Num?) #t]
  [(⊑/subject Pos? Nat?) #t]
  [(⊑/subject Pos? Num?) #t]
  [(⊑/subject named_0 named_1) #f]
  ;; Function Contract
  [(⊑/subject (C_0 → D_0) (C_1 → D_1)) ,(and (term (⊑/context C_0 C_1)) (implies (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 D_1))))]
  ;; Intersection Contract
  [(⊑/subject (C_0 ∩ D_0) C_1) ,(or (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 C_1)))]
  [(⊑/subject C_0 (C_1 ∩ D_1)) ,(and (term (⊑/subject C_0 C_1)) (term (⊑/subject C_0 D_1)))]
  ;; Union Contract
  [(⊑/subject (C_0 ∪ D_0) C_1) ,(and (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 C_1)))]
  [(⊑/subject C_0 (C_1 ∪ D_1)) ,(or (term (⊑/subject C_0 C_1)) (term (⊑/subject C_0 D_1)))]
  ;; Dependent Contract
  ;; TODO
  )



;; Blame Calculation
;; =================


(define-metafunction λCon
  π : B -> ω
  [(π #t) (#t ∘ #t)]
  [(π #f) (#t ∘ #f)])

(define-metafunction λCon
  subject : ω -> B
  [(subject (B_0 ∘ B_1)) B_1])

(define-metafunction λCon
  context : ω -> B
  [(context (B_0 ∘ B_1)) B_0])

(define-metafunction λCon
  μ : ς b -> ω
  [(μ ((b_0 ◃ κ) ς) b_0) (solve ((b_0 ◃ κ) ς) κ)]
  [(μ ((b_0 ◃ κ) ς) b_1) (μ ς b_1)])

(define-metafunction λCon
  solve : ς κ -> ω
  
  [(solve ς (ι_0 → ι_1)) (solve/→ (μ ι_0) (μ ι_1))]
  [(solve ς (ι_0 ∩ ι_1)) (solve/∩ (μ ι_0) (μ ι_1))]
  [(solve ς (ι_0 ∪ ι_1)) (solve/∪ (μ ι_0) (μ ι_1))]
  
  [(solve ς ι) (μ ι)]
  [(solve ς B) (π B)]
  )

(define-metafunction λCon
  solve/→ : ω ω -> ω
  [(solve/→ ω_0 ω_1) ((and (subject ω_0) (context ω_1)) ∘ (and (context ω_0) (implies (subject ω_0) (subject ω_1))))])

(define-metafunction λCon
  solve/∩ : ω ω -> ω
  [(solve/∩ ω_0 ω_1) ((or (context ω_0) (context ω_1)) ∘ (and (subject ω_0) (subject ω_1)))])

(define-metafunction λCon
  solve/∪ : ω ω -> ω
  [(solve/∪ ω_0 ω_1) ((and (context ω_0) (context ω_1)) ∘ (or (subject ω_0) (subject ω_1)))])


(define-metafunction λCon
  is-blame-state? : ς -> B
  [(is-blame-state? ((♭ ◃ κ) ς)) (or (μ ((♭ ◃ κ) ς) ♭) (is-blame-state? ς))]
  [(is-blame-state? ((ι ◃ κ) ς)) (is-blame-state? ς)]
  [(is-blame-state? ·) #f])


;(redex-match? λCon ♭ (term ♭1))
;(variable-not-in (term (+ ♭1 ♭1)) (term ♭))
;(variable-not-in (term (+ ι y)) (term ι))
;(fresh (term (+ ι y)))


(define false? 
  (redex-match? λCon false))

(define λCon-value?
  (redex-match? λCon V))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define λCon-reduction
  (extend-reduction-relation 
   λJ-reduction
   λCon
   ;   (--> (in-hole E (assert v C))
   ;        (in-hole E (v @ C))
   ;        "Assert"
   ;   )
   ;; Immediate Contarcts
   (--> (in-hole E (V @ (flat M)))
        (in-hole E (V @ (eval (M V))))
        "Flat"
        )
   (--> (in-hole E (V @ (eval W)))
        (in-hole E V)
        "Unit"
        (side-condition (not (false? (term W))))
        ;(side-condition (not (equal? (term W) #f)))
        )
   (--> (in-hole E (V @ (eval W)))
        (in-hole E V)
        ;;blame ;; TODO, Change to V and introduce top-level blame
        "Blame"
        (side-condition (false? (term W)))
        )
   (--> (in-hole E (V @ (C ∪ D)))
        (in-hole E ((V @ C) @ D))
        "Union"
        )
   (--> (in-hole E (V @ (I ∩ C)))
        (in-hole E ((V @ I) @ C))
        "Intersection"
        )
   ;; Delayed Contarcts
   (--> (in-hole E ((V @ (C → D)) W))
        (in-hole E ((V (W @ C)) @ D))
        "D-Function"
        )
   (--> (in-hole E ((V @ (x → C)) W)) ;; TODO
        (in-hole E ((V W) @ C))
        "D-Dependent"
        )
   (--> (in-hole E ((V @ (Q ∩ R)) W))
        (in-hole E (((V @ Q) @ R) W))
        "D-Intersection"
        )
   
   
   ;; Lookup
   (--> (in-hole E (V @ named))
        (in-hole E (V @ (lookup named)))
        "Lookup"
        )
   ))

(define-metafunction λCon
  lookup : named -> I
  [(lookup ⊤) (flat (λ x #t))]
  [(lookup ⊥) (flat (λ x #f))]
  
  [(lookup Any?) (flat (λ x #t))]
  [(lookup None?) (flat (λ x #f))]
  
  [(lookup Num?) (flat (λ x (number? x)))]
  [(lookup Str?) (flat (λ x (string? x)))]
  [(lookup Bool?) (flat (λ x (boolean? x)))]
  
  [(lookup Pos?) (flat (λ x (> x 0)))]
  [(lookup Nat?) (flat (λ x (or (> x 0) (= x 0))))]
  [(lookup Neg?) (flat (λ x (< x 0)))]
  )



(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))