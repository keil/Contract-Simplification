#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

(require "lift.rkt")
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

(define-extended-language λCon-Subset λCon-Lift 
 
   ((ACtx) hole (ACtx @ b C))   
 
)

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Subset Reduction
;; ================
;; 

;; or callit predicate redution

;; is it possible to reuse teh same context ?

(define Subset-reduction
  (extend-reduction-relation
   Lift-reduction
   λCon-Subset
   #:domain (ς any)
   
   ;(--> (ς
   ;      (in-hole F ((T @ ι_0 I) @ ι_1 Q)))
   ;     (ς
   ;      (in-hole F ((T @ ι_1 Q) @ ι_0 I)))
   ;     "SwitchXXX")
   
   ;; DO i need to update the constaint grapg
   ;; is this level1 (order) blame perserving
   ;; or ordre change blame perserving
   ;; IF its not the direct context
   
   (--> (ς
         (in-hole F ((T @ ι_0 C) @ ι_1 D)))
        (ς
         (in-hole F (T @ ι_0 C)))
        "Subset1"
        (side-condition (and
                         (term (⊑ C D))
                         (canonical? (term (in-hole F ((T @ ι_0 C) @ ι_1 D))))
                         )))
   
   
   (--> (ς
         (in-hole F ((T @ ι_0 C) @ ι_1 D)))
        (ς
         (in-hole F (T @ ι_1 D)))
        "Subset2"
        (side-condition (and
                         (term (⊑ D C))
                         (canonical? (term (in-hole F ((T @ ι_0 C) @ ι_1 D))))
                        )))
   
   
   

   
   
   
   ;   (--> (ς
   ;         (in-hole F ((T @ ι_0 C) @ ι_1 D)))
   ;        (ς
   ;         (in-hole F ((T @ (ι_0 ι_1) C) @ ι_1 (\\ D C))))
   ;        "Subset"
   ;        (side-condition (not (λCon-value? (term B)))))
   
   ))

;; reduction when in pos in pos cap even














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
;(define canonical?
;  (redex-match λCon-Baseline T))

;; Reducible? (non-canonical terms)
;; --------------------------------
;(define reducible? 
;  (redex-match λCon-Baseline Reducible))

;; Final? (top-level final terms)
;; ------------------------------
;(define final? 
;  (redex-match λCon-Baseline Final))



;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Subset~~> ς M)
  (if (redex-match? λCon M M)
      (car (apply-reduction-relation Baseline-reduction (term (,ς ,M))))
      (error "Invalid λCon-term:" M)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Subset~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Baseline-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))