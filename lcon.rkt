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
  
  ;; Predicates (Refienments)
  ;; ------------------------
  (P ⊤ (P / M) predefined)
  
  (predefined %Number %Complex %Real %Rational %Integer %String %Boolean
              %Exact %Inexact %Zero  
              %Positive %Negative %Even %Odd %Natural  
              %UInteger %UEven %UOdd)
  
  
  
  ;; Contracts
  ;; ---------
  
  ;; Contracts
  ((C D) I Q A (C ∪ D) (I ∩ C))
  
  ;; Contract Abstraction
  (A (Λ x C))
  
  ; Immediate Contracts
  ((I J) (flat P ...))
  
  ; Delayed Contracts
  ((Q R) (C → D) (x ↦ (A x)) (Q ∩ R))
  
  
  
  ;; λCon Extention
  ;; --------------
  
  ;; Blame
  (blame +blame -blame)
  
  ;; Values
  ((U V W) .... ((λ x M) @ Q) blame)
  
  ;; Terms
  ((L M N) .... (M @ C) blame)
  
  ;; Contexts
  (E .... (E @ C) (V @ (eval E)))
  
  
  
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
  (ω (B_0 ∘ B_1)))

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
   #:domain (ς M)
   
   ;; Contract Assertion
   (--> (ς
         (in-hole E (V @ C)))
        (ς
         (in-hole E (V @ ♭ C)))
        "Assert")
   
   ;; Immediate Contarcts
   (--> (ς
         (in-hole E (V @ (flat M))))
        (ς
         (in-hole E (V @ (eval ,(with-handlers 
                                    ([(λ x #t) (lambda (exn) (term #f))])
                                  (evaluate (term (M V))))))))
        "Flat")
   
   (--> (ς
         (in-hole E (V @ P)))
        (ς
         (in-hole E (V @ (eval (eval/ (Σ P) V)))))
        "Refinement")
   
   (--> (ς
         (in-hole E (V @ (eval W))))
        ((b ◃ W) ς) ;; TODO
        (in-hole E V))
   "Unit"
   (side-condition (not (false? (term W)))))
  
  
  
  (--> (ς 
        (in-hole E (V @ b (C ∪ D))))
       (((b ◃ (ι_1 ∪ ι_2)) ς)
        (in-hole E ((V @ ι_1 C) @ ι_2 D)))
       "Union")
  
  (--> (ς
        (in-hole E (V @ b (I ∩ C))))
       (((b ◃ (ι_1 ∩ ι_2)) ς)
        (in-hole E ((V @ ι_1 I) @ ι_2 C)))
       "Intersection")
  
  ;; Delayed Contarcts
  (--> (ς
        (in-hole E ((V @ b (C → D)) W)))
       (((b ◃ (ι_1 → ι_2)) ς)
        (in-hole E ((V (W @ ι_1 C)) @ ι_2 D)))
       "D-Function")
  
  (--> (ς
        (in-hole E ((V @ b (x → (Λ x C))) W)))
       (ς
        (in-hole E ((V W) @ b (subst/ x W C))))
       "D-Dependent")
  
  (--> (ς
        (in-hole E ((V @ b (Q ∩ R)) W)))
       (((b ◃ (ι_1 ∩ ι_2)) ς)
        (in-hole E (((V @ ι_1 Q) @ ι_2 R) W)))
       "D-Intersection")
  
  
  (--> (ς
        (in-hole E (V @ (eval W))))
       (ς
        (in-hole E V))
       "Blame"
       (side-condition (false? (term W))))
  
  
  ;; TODO, abstraction
  
  ;; Lookup
  ;(--> (in-hole E (V @ named))
  ;     (in-hole E (V @ (lookup named)))
  ;     "Lookup") ;; TODO
  ;
  ;(--> (in-hole E (V @ predefined))
  ;     (in-hole E (V @ (lookup/ predefined)))
  ;     "Lookup/")
  ; 
  
  ;(--> (in-hole E (V @ ⊤))
  ;     (in-hole E V)
  ;     "⊤")
  ;)
  )
















#|
 ___            _      __ _             _ 
| _ \_ _ ___ __| |___ / _(_)_ _  ___ __| |
|  _/ '_/ -_) _` / -_)  _| | ' \/ -_) _` |
|_| |_| \___\__,_\___|_| |_|_||_\___\__,_|
                                          
 ___            _ _         _          
| _ \_ _ ___ __| (_)__ __ _| |_ ___ ___
|  _/ '_/ -_) _` | / _/ _` |  _/ -_|_-<
|_| |_| \___\__,_|_\__\__,_|\__\___/__/
                                                     
|#

;; Lookup (Predefined Predicates)
;; ==============================

(define-metafunction λCon
  lookup : predefined -> P
  
  ;; First Level
  [(lookup %Number)   (⊤ / (λ x (number? x)))]
  [(lookup %Complex)  (⊤ / (λ x (complex? x)))]
  [(lookup %Real)     (⊤ / (λ x (real? x)))]
  [(lookup %Rational) (⊤ / (λ x (rational? x)))]
  [(lookup %Integer)  (⊤ / (λ x (integer? x)))]
  
  [(lookup %String)   (⊤ / (λ x (string? x)))]
  [(lookup %Boolean)  (⊤ / (λ x (boolean? x)))]
  
  ;; Second Level
  [(lookup %Exact)   (Number? / (λ x (exact? x)))]
  [(lookup %Inexact) (Number? / (λ x (inexact? x)))]
  [(lookup %Zero)    (Number? / (λ x (zero? x)))]
  
  [(lookup %Positive) (Real? / (λ x (positive? x)))]
  [(lookup %Negative) (Real? / (λ x (negative? x)))]
  [(lookup %Natural)  (Real? / (λ x (<= x 0)))]
  
  [(lookup %Even)     (Integer? / (λ x (even? x)))]
  [(lookup %Odd)      (Integer? / (λ x (odd? x)))]
  [(lookup %UInteger) (Integer? / (λ x (<= x 0)))]
  
  ;; Third Level
  [(lookup %UEven) (UInteger? / (λ x (even? x)))]
  [(lookup %UOdd)  (UInteger? / (λ x (odd? x)))])



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
;; Returns true if both terms are syntactically identical (after α conversion), 
;; false otherwise.

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
  ≼ : (λ x M) (λ x M) -> boolean
  [(≼ (λ x (complex? x))  (λ x (number? x)))   #t]
  [(≼ (λ x (real? x))     (λ x (number? x)))   #t]
  
  [(≼ (λ x (rational? x)) (λ x (real? x)))     #t]
  [(≼ (λ x (rational? x)) (λ x (number? x)))     #t]
  
  [(≼ (λ x (integer? x))  (λ x (rational? x))) #t]
  [(≼ (λ x (integer? x))  (λ x (real? x))) #t]
  [(≼ (λ x (integer? x))  (λ x (number? x))) #t]
  
  [(≼ (λ x (positive? x)) (λ x (<= x 0)))      #t]
  ;; Otherwise
  [(≼ any ...) (≡ any ...)])







;; Sum (Σ)
;; ---------------------------------
;; Summarizes the patricular predicates of a refinement.

(define-metafunction λCon
  ⊕ : (M ...) (M ...) -> (M ...)
  [(⊕ (M ...) ()) (M ...)]
  [(⊕ () (M ...)) (M ...)]
  [(⊕ (M_0 ... M_n M_i ...) (M_n M_m ...)) (⊕ (M_0 ... M_n M_i ...) (M_m ...))]
  [(⊕ (M_0 ...) (M_n M_m ...)) (⊕ (M_0 ... M_n) (M_m ...))])

(define-metafunction λCon
  Σ : P -> (M ...)
  [(Σ ⊤) ((λ x #t))]
  [(Σ ⊥) ((λ x #f))]
  ;[(Σ (⊤ / M)) (M)]
  [(Σ (P / M)) (⊕ (Σ P) (M))]
  [(Σ predefined) (Σ (lookup/ predefined))]
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

(define λCon-value?
  (redex-match? λCon V))

(define-metafunction λCon
  subst/ : x any any -> any
  [(subst/ x any (Λ x C)) (Λ x M)]
  [(subst/ x any (Λ y C)) (Λ y (subst/ x any C))]
  [(subst/ x any ...) (subst x any ...)])


(define-metafunction λCon
  eval/ : (M ...) V -> boolean
  [(eval/ () V) #t]
  ;  [(eval/ (⊤) V) #t]
  ;  [(eval/ (⊥) V) #f]
  [(eval/ (M) V) ,(with-handlers ([(λ x #t) (lambda (exn) (term #f))]) (evaluate (term (M V))))]
  ;  [(eval/ (M_0 M_1 ... M_n) V) ,(and (term (eval/ M_n V)))]
  [(eval/ (M_0 M_1 ...) V) ,(and (term (eval/ (M_0) V)) (term (eval/ (M_1 ...) V)))]
  [(eval/ any ...) #f]
  )



(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))













