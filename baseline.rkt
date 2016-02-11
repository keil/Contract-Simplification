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
  ((I J) .... ⊥ ⊤) ;; TODO, JCon needs to knwo ⊤/⊥
  
  ;; Delayed Contracts
  ;; -----------------
  ((Q R) .... (C → ⊤)) ;; TODO (⊤ → C) can be reduces
  
  
  
  
  ;; Contract-free terms (λJ terms)
  ;; ------------------------------
  ((S T) K x (λ x T) (S T) (op T ...) (if S T_0 T_1))
  
  ;; Non-reducable terms (stucked assertions)
  ;; ----------------------------------------
  ((A B) 
   ;; Contract-free terms
   T
   ;; Imemdiate Contracts
   ((A B) @ ι I)
   ((op B ...) @ ι I)
   ((if A B_0 B_1) @ ι I)
   
   ;; Blame terms
   (blame ♭))
  
  ;; Baseline Reduction Context
  ;; --------------------------
  (F hole (λ x F) (F M) (B F) (op B ... F M ...) (if F M N) (if A F N) (if A B F) (F @ b C)) 
  
  ;; Miscellaneous
  ;; -------------
  
  ;; True-Contracts
  (true ⊤ (true → true)))

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
         (in-hole F (B @ ♭ C)))
        (((♭ ◃ ι) ς)
         (in-hole F (B @ ι C)))
        "Unfold/Assert"
        (fresh ι))
   
   (--> (ς
         (in-hole F (M @ ι (C ∪ D))))
        (((ι ◃ (ι1 ∪ ι2)) ς)
         (in-hole F ((M @ ι1 C) @ ι2 D)))
        "Unfold/Union"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (M @ ι (I ∩ C))))
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F ((M @ ι1 I) @ ι2 C)))
        "Unfold/Intersection"
        (fresh ι1 ι2))
   
   ;; Valid Contracts
   ;; ---------------
   
   (--> (ς
         (in-hole F (K @ ι Q)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F K))
        "Reduce/Constant")
   
   (--> (ς
         (in-hole F ((op M ...) @ ι Q)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F (op M ...)))
        "Redcude/Operation")
   
   (--> (ς
         (in-hole F (V @ ι true)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F V))
        "Recude/True")
   
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



(define Subset-evaluation
  (reduction-relation
   λCon-Baseline
   #:domain (ς any)
   
   
   ))



;; Baseline Reduction
;; ------------------
;; Verifies all (immediate) contracts 
;; that can be check at compile time

;; TODO
;; implement top-level blame rule











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
  [(≡ (λ x M) (λ y N)) (≼ (λ z (subst x z M)) (λ z (subst y z N)))
                       (where z ,(variable-not-in (term ((λ x M) (λ y N))) (term z)))]
  ;; Otherwise
  [(≡ any ...) #f])


;; Term Subset (≼)
;; ---------------
;; This meta function models the implicite assertions of predicates. Some of the 
;; realtions could be might be determinable by unsing a SAT solvers.
;; Returns true if the left term is subset or equals to the reight term, 
;; false otherwise.

(define-metafunction λCon
  ≤ : (λ x M) (λ x M) -> boolean
  ;; complex? ≤
  [(≼ (λ x (complex? x))  (λ x (number? x)))   #t]
  
  [(≼ (λ x (real? x))     (λ x (number? x)))   #t]
  
  [(≼ (λ x (rational? x)) (λ x (real? x)))     #t]
  [(≼ (λ x (rational? x)) (λ x (number? x)))   #t]
  
  [(≼ (λ x (integer? x))  (λ x (rational? x))) #t]
  [(≼ (λ x (integer? x))  (λ x (real? x)))     #t]
  [(≼ (λ x (integer? x))  (λ x (number? x)))   #t]
  
  
  
  
  [(≼ (λ x (positive? x)) (λ x (<= x 0)))      #t]
  ;; Otherwise
  [(≼ any ...) (≡ any ...)])









