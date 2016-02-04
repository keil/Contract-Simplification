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
  ((U V W) .... ((λ x M) @ b Q) (blame ♭))
  
  ;; Terms
  ((L M N) .... (M @ C) (M @ b C) blame)
  
  ;; Contexts
  (E .... (E @ C) (E @ b C))
  
  
  
  ;; Blame Constraints
  ;; -----------------
  
  ;; blame labels
  (♭ (variable-prefix ♭))
  
  ;; blame variales
  (ι (variable-prefix ι))
  
  ;; blame identifiers
  (b ♭ ι)
  
  ;; boolean values
  (B #t #f)
  
  ;; Constraints
  (κ ω ι (ι_1 → ι_2) (ι_1 ∩ ι_2) (ι_1 ∪ ι_2))
  
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
   #:domain (ς any)
   
   ;; λJ Reduction
   (--> (ς
         (in-hole E (op V ...)))
        (ς
         (in-hole E (δ op V ...)))
        "δ")
   
   (--> (ς
         (in-hole E ((λ x M) V)))
        (ς
         (in-hole E (subst x V M)))
        "β")
   
   (--> (ς
         (in-hole E (if V M N)))
        (ς
         (in-hole E M))
        "if/true"
        (side-condition (not (false? (term V)))))
   
   (--> (ς
         (in-hole E (if V M N)))
        (ς
         (in-hole E N))
        "if/false"
        (side-condition (false? (term V))))
  
  ;; Contract Assertion
  (--> (ς
        (in-hole E (V @ C)))
       (ς
        (in-hole E (V @ ♭ C)))
       "Assert")
  
  ;; Immediate Contarcts
  (--> (ς
        (in-hole E (V @ b (flat P ...))))
       (ς
        (in-hole E (V @ b (eval (Σ P ...) V))))
       "Flat")
  
  (--> (ς
        (in-hole E (V @ b W)))
       (((b ◃ (π W)) ς)
        (in-hole E V))
       "Unit")
  
  (--> (ς 
        (in-hole E (V @ b (C ∪ D))))
       (((b ◃ (ι1 ∪ ι2)) ς)
        (in-hole E ((V @ ι1 C) @ ι2 D)))
       "Union"
       (fresh ι1 ι2))
  
  (--> (ς
        (in-hole E (V @ b (I ∩ C))))
       (((b ◃ (ι1 ∩ ι2)) ς)
        (in-hole E ((V @ ι1 I) @ ι2 C)))
       "Intersection"
       (fresh ι1 ι2))
  
  ;; Delayed Contarcts
  (--> (ς
        (in-hole E ((V @ b (C → D)) W)))
       (((b ◃ (ι1 → ι2)) ς)
        (in-hole E ((V (W @ ι1 C)) @ ι2 D)))
       "D-Function"
       (fresh ι1 ι2))
  
  (--> (ς
        (in-hole E ((V @ b (x → (Λ x C))) W)))
       (ς
        (in-hole E ((V W) @ b (subst/ x W C))))
       "D-Dependent")
  
  (--> (ς
        (in-hole E ((V @ b (Q ∩ R)) W)))
       (((b ◃ (ι1 ∩ ι2)) ς)
        (in-hole E (((V @ ι1 Q) @ ι2 R) W)))
       "D-Intersection"
       (fresh ι1 ι2))
  
  ))
;  (--> (ς
;        (in-hole E (V @ (eval W))))
;       (ς
;        (in-hole E V))
;       "Blame"
;       (side-condition (false? (term W))))


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
  [(lookup %Exact)   (%Number / (λ x (exact? x)))]
  [(lookup %Inexact) (%Number / (λ x (inexact? x)))]
  [(lookup %Zero)    (%Number / (λ x (zero? x)))]
  
  [(lookup %Positive) (%Real / (λ x (positive? x)))]
  [(lookup %Negative) (%Real / (λ x (negative? x)))]
  [(lookup %Natural)  (%Real / (λ x (<= x 0)))]
  
  [(lookup %Even)     (%Integer / (λ x (even? x)))]
  [(lookup %Odd)      (%Integer / (λ x (odd? x)))]
  [(lookup %UInteger) (%Integer / (λ x (<= x 0)))]
  
  ;; Third Level
  [(lookup %UEven) (%UInteger / (λ x (even? x)))]
  [(lookup %UOdd)  (%UInteger / (λ x (odd? x)))])

#|
 ___            _ _         _         ___          _           _   _          
| _ \_ _ ___ __| (_)__ __ _| |_ ___  | __|_ ____ _| |_  _ __ _| |_(_)___ _ _  
|  _/ '_/ -_) _` | / _/ _` |  _/ -_) | _|\ V / _` | | || / _` |  _| / _ \ ' \ 
|_| |_| \___\__,_|_\__\__,_|\__\___| |___|\_/\__,_|_|\_,_\__,_|\__|_\___/_||_|
                                                                              
|#

;; Sum (Σ)
;; -------
;; Summarizes the patricular predicates of a
;; refinement chain (P).
(define-metafunction λCon
  Σ : P -> (M ...)
  [(Σ ⊤) ()]
  [(Σ predefined) (Σ (lookup predefined))]
  [(Σ (P / M)) (⊕ (Σ P) (M))])

;; Predicate Evaluation (eval)
;; ---------------------------
;; Evaluates a set oof predicates and
;; returns the conjunction of the results.
(define-metafunction λCon
  eval : (M ...) V -> V
  [(eval () V) #t]
  [(eval (M) V) (↓ ,(with-handlers ([(λ x #t) (lambda (exn) (term #f))]) (λCon-->* (term (M V)))))]
  [(eval (M_0 M_1 ...) V) ,(and (term (eval (M_0) V)) (term (eval (M_1 ...) V)))]
  [(eval any ...) #f])

#|
 __  __     _            ___             _   _             
|  \/  |___| |_ __ _ ___| __|  _ _ _  __| |_(_)___ _ _  ___
| |\/| / -_)  _/ _` |___| _| || | ' \/ _|  _| / _ \ ' \(_-<
|_|  |_\___|\__\__,_|   |_| \_,_|_||_\__|\__|_\___/_||_/__/

|#

;; Union (⊎)
;; ---------
;; Merges two sets
(define-metafunction λCon
  ⊎ : (any ...) (any ...) -> (any ...)
  [(⊎ (any_l ...) (any_r ...)) (any_l ... any_r ...)])

;; Set Union (⊕)
;; ---------
;; Merges two sets and avoids double entries.
(define-metafunction λCon
  ⊕ : (any ...) (any ...) -> (any ...)
  [(⊕ (any ...) ()) (any ...)]
  [(⊕ () (any ...)) (any ...)]
  [(⊕ (any_0 ... any_i any_n ...) (any_i any_m ...)) (⊕ (any_0 ... any_i any_n ...) (any_m ...))]
  [(⊕ (any_0 ...) (any_i any_m ...)) (⊕ (any_0 ... any_i) (any_m ...))])

;; Term of (↓)
;; -----------
;; Returns the term contain in a configuration.
(define-metafunction λCon
  ↓ : (ς M) -> M
  [(↓ (ς M)) M])

#|
 ___ _                   ___      _         _      _   _          
| _ ) |__ _ _ __  ___   / __|__ _| |__ _  _| |__ _| |_(_)___ _ _  
| _ \ / _` | '  \/ -_) | (__/ _` | / _| || | / _` |  _| / _ \ ' \ 
|___/_\__,_|_|_|_\___|  \___\__,_|_\__|\_,_|_\__,_|\__|_\___/_||_|
                                                                  
|#

;; Solution (π)
;; ------------
;; Function π mapps boolean values to a solution (context ∘ subject).
(define-metafunction λCon
  π : B -> ω
  [(π #t) (#t ∘ #t)]
  [(π #f) (#t ∘ #f)])

;; Subject
;; -------
;; Retuns the subject part of a solution (context ∘ subject).
(define-metafunction λCon
  subject : ω -> B
  [(subject (B_0 ∘ B_1)) B_1])

;; Context
;; --------
;; Retuns the context part of a solution (context ∘ subject).
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




(define-metafunction λCon
  subst/ : x any any -> any
  [(subst/ x any (Λ x C)) (Λ x M)]
  [(subst/ x any (Λ y C)) (Λ y (subst/ x any C))]
  [(subst/ x any ...) (subst x any ...)])

















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

;; λCon-Value (λCon-value?)
;; ------------------------
(define λCon-value?
  (redex-match? λCon V))

;; λCon-Term (λCon-term?)
;; ----------------------
(define λCon-term?
  (redex-match? λCon M))

;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon--> M)
  (car (apply-reduction-relation λCon-reduction (term (· ,M)))))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon-->* M)
  (car (apply-reduction-relation* λCon-reduction (term (· ,M)))))