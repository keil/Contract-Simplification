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
  ((C D) I Q (C ∪ D) (I ∩ C))
  
  ;; Contract Abstraction
  (A (Λ x C))
  
  ; Immediate Contracts
  ((I J) (flat P ...))
  
  ; Delayed Contracts
  ((Q R) (C → D) (x ↦ A) (Q ∩ R))
  
  
  
  ;; λCon Extention
  ;; --------------
  
  ;; Blame
  (blame +blame -blame)
  
  ;; Values
  ((U V W) .... (V @ b Q) (blame ♭))
  
  ;; Terms
  ((L M N) .... (M @ C) (M @ b C))
  
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
        "δ"
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E ((λ x M) V)))
        (ς
         (in-hole E (subst x V M)))
        "β"
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E (if V M N)))
        (ς
         (in-hole E M))
        "if/true"
        (side-condition (not (false? (term V))))
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E (if V M N)))
        (ς
         (in-hole E N))
        "if/false"
        (side-condition (false? (term V)))
        (side-condition (not (term (is-blame-state? ς)))))
   
   ;; Contract Assertion
   (--> (ς
         (in-hole E (V @ C)))
        (ς
         (in-hole E (V @ ♭ C)))
        "Assert"
        (fresh ♭)
        (side-condition (not (term (is-blame-state? ς)))))
   
   ;; Immediate Contarcts
   (--> (ς
         (in-hole E (V @ b (flat P ...))))
        (ς
         (in-hole E (V @ b (eval (Σ P ...) V))))
        "Flat"
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E (V @ b W)))
        (((b ◃ (τ W)) ς)
         (in-hole E V))
        "Unit"
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς 
         (in-hole E (V @ b (C ∪ D))))
        (((b ◃ (ι1 ∪ ι2)) ς)
         (in-hole E ((V @ ι1 C) @ ι2 D)))
        "Union"
        (fresh ι1 ι2)
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E (V @ b (I ∩ C))))
        (((b ◃ (ι1 ∩ ι2)) ς)
         (in-hole E ((V @ ι1 I) @ ι2 C)))
        "Intersection"
        (fresh ι1 ι2)
        (side-condition (not (term (is-blame-state? ς)))))
   
   ;; Delayed Contarcts
   (--> (ς
         (in-hole E ((V @ b (C → D)) W)))
        (((b ◃ (ι1 → ι2)) ς)
         (in-hole E ((V (W @ ι1 C)) @ ι2 D)))
        "D-Function"
        (fresh ι1 ι2)
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E ((V @ b (x ↦ (Λ x C))) W)))
        (ς
         (in-hole E ((V W) @ b (subst/ x W C))))
        "D-Dependent"
        (side-condition (not (term (is-blame-state? ς)))))
   
   (--> (ς
         (in-hole E ((V @ b (Q ∩ R)) W)))
        (((b ◃ (ι1 ∩ ι2)) ς)
         (in-hole E (((V @ ι1 Q) @ ι2 R) W)))
        "D-Intersection"
        (fresh ι1 ι2)
        (side-condition (not (term (is-blame-state? ς)))))
   
   ;; Blame
   (--> (ς
         any)
        (ς
         (blame ♭))
        "Blame"
        (side-condition (not (blame? (term any))))
        (side-condition (term (is-blame-state? ς)))
        (where (blame ♭) (produce-blame  ς)))
   
   
   ))

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
  [(eval (M) V) (⇓/Term ,(with-handlers ([(λ x #t) (lambda (exn) (term (· #f)))]) (λCon-->* (term (M V)))))]
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

;; Term of (⇓/Term)
;; ----------------
(define-metafunction λCon
  ⇓/Term : (ς M) -> M
  [(⇓/Term (ς M)) M])

;; State of (⇓/State)
;; ------------------
(define-metafunction λCon
  ⇓/State : (ς M) -> ς
  [(⇓/State (ς M)) ς])

;; Number of Blame Labels (count/♭)
;; --------------------------------
(define-metafunction λCon
  count/♭ : ς -> number
  [(count/♭ ·) 0]
  [(count/♭ ((♭ ◃ κ) ς)) ,(+ 1 (term (count/♭ ς)))]
  [(count/♭ ((b ◃ κ) ς)) (count/♭ ς)])

;; Number of Blame Variables (count/ι)
;; -----------------------------------
(define-metafunction λCon
  count/ι : ς -> number
  [(count/ι ·) 0]
  [(count/ι ((ι ◃ κ) ς)) ,(+ 1 (term (count/ι ς)))]
  [(count/ι ((b ◃ κ) ς)) (count/♭ ς)])

;; Number of Blame Identifiers (count/b)
;; -------------------------------------
(define-metafunction λCon
  count/b : ς -> number
  [(count/b ·) 0]
  [(count/b ((b ◃ κ) ς)) ,(+ 1 (term (count/b ς)))])

#|
 ___ _                   ___      _         _      _   _          
| _ ) |__ _ _ __  ___   / __|__ _| |__ _  _| |__ _| |_(_)___ _ _  
| _ \ / _` | '  \/ -_) | (__/ _` | / _| || | / _` |  _| / _ \ ' \ 
|___/_\__,_|_|_|_\___|  \___\__,_|_\__|\_,_|_\__,_|\__|_\___/_||_|
                                                                  
|#

;; Solution (τ)
;; ------------
;; Function τ mapps boolean values to a solution (context ∘ subject).
(define-metafunction λCon
  τ : B -> ω
  [(τ #t) (#t ∘ #t)]
  [(τ #f) (#t ∘ #f)])

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

;; Labels
;; --------
;; Retuns all blame labels of a constraint set ς.
(define-metafunction λCon
  labels : ς -> (♭ ...)
  [(labels ((♭ ◃ κ) ς)) (⊕ (♭) (labels ς))]
  [(labels ((ι ◃ κ) ς)) (labels ς)]
  [(labels ·) ()])

#|
 ___ _                  ___ _        _       
| _ ) |__ _ _ __  ___  / __| |_ __ _| |_ ___ 
| _ \ / _` | '  \/ -_) \__ \  _/ _` |  _/ -_)
|___/_\__,_|_|_|_\___| |___/\__\__,_|\__\___|
                                             
|#

;; Is Blame State
;; --------------
;; Checks if there exists any blame label ♭ such that ς
;; is a blame state for ♭.
(define-metafunction λCon
  is-blame-state? : ς -> boolean
  [(is-blame-state? ς) (check-labels ς (labels ς))])

;; Check Label
;; -----------
;; Checks the Blame State for a set of labels.
(define-metafunction λCon
  check-labels : ς (♭ ...) -> boolean
  [(check-labels ς ()) #f]
  [(check-labels ς (♭_0 ♭_1 ...)) ,(or (term (is-blame-state-for? ς ♭_0))
                                       (term (check-labels ς (♭_1 ...))))])
;; Is Blame State For
;; ------------------
;; Checks if ς is a blame state for ♭.
(define-metafunction λCon
  is-blame-state-for? : ς ♭ -> boolean
  [(is-blame-state-for? ς ♭) (maps-to-false? (μ ς ♭))])

;; Is False
;; --------
;; Checks if a solution maps to false.
(define-metafunction λCon
  maps-to-false? : ω -> boolean
  [(maps-to-false? (B_0 ∘ B_1)) ,(nand (term B_0) (term B_1))])

#|
 ___             _               ___ _                
| _ \_ _ ___  __| |_  _ __ ___  | _ ) |__ _ _ __  ___ 
|  _/ '_/ _ \/ _` | || / _/ -_) | _ \ / _` | '  \/ -_)
|_| |_| \___/\__,_|\_,_\__\___| |___/_\__,_|_|_|_\___|
     
|#

;; Produce Blame
;; -------------
;; Produces a source blame from a state (in blame state).
(define-metafunction λCon
  produce-blame : ς -> any
  [(produce-blame ς) (to-blame ς (labels ς))])

;; To Blame
;; --------
;; Looks for a label to blame.
(define-metafunction λCon
  to-blame : ς (♭ ...) -> any
  [(to-blame ς (♭_0 ♭_1 ...)) ,(if (term (is-blame-state-for? ς ♭_0))
                                   (term (produce-blame-for (μ ς ♭_0) ♭_0))
                                   (term (to-blame ς (♭_1 ...))))])

;; Produce Balme
;; -------------
;; Converts a solution and a blame lable to a source blame.
(define-metafunction λCon
  produce-blame-for : ω ♭ -> (blame ♭)
  [(produce-blame-for (#f ∘ B) ♭) (-blame ♭)]
  [(produce-blame-for (B ∘ #f) ♭) (+blame ♭)])

#|
  ___             _            _     _   
 / __|___ _ _  __| |_ _ _ __ _(_)_ _| |_ 
| (__/ _ \ ' \(_-<  _| '_/ _` | | ' \  _|
 \___\___/_||_/__/\__|_| \__,_|_|_||_\__|
                                         
 ___       _   _     __         _   _          
/ __| __ _| |_(_)___/ _|__ _ __| |_(_)___ _ _  
\__ \/ _` |  _| (_-<  _/ _` / _|  _| / _ \ ' \ 
|___/\__,_|\__|_/__/_| \__,_\__|\__|_\___/_||_|
                                               
|#

;; Constraint Lookup
;; ----------------
(define-metafunction λCon
  π : ς b -> κ
  [(π ((b_0 ◃ κ) ς) b_0) κ]
  [(π ((b_0 ◃ κ) ς) b_1) (π ς b_1)]
  [(π any ...) (#t ∘ #t)]
  )

;; Compute Solution
;; ----------------
(define-metafunction λCon
  μ : ς b -> ω
  [(μ ς b) (solve ς (π ς b))])

;; Solve Constraint
;; ----------------
(define-metafunction λCon
  solve : ς κ -> ω
  [(solve ς (ι_0 → ι_1)) (solve/→ (μ ς ι_0) (μ ς ι_1))]
  [(solve ς (ι_0 ∩ ι_1)) (solve/∩ (μ ς ι_0) (μ ς ι_1))]
  [(solve ς (ι_0 ∪ ι_1)) (solve/∪ (μ ς ι_0) (μ ς ι_1))]
  [(solve ς ι) (μ ς ι)]
  [(solve ς ω) ω])

;; Solve Function
;; --------------
(define-metafunction λCon
  solve/→ : ω ω -> ω
  [(solve/→ ω_0 ω_1) (,(and (term (subject ω_0)) (term (context ω_1))) ∘ ,(and (term (context ω_0)) (implies (term (subject ω_0)) (term (subject ω_1)))))])

;; Solve Union
;; -----------
(define-metafunction λCon
  solve/∪ : ω ω -> ω
  [(solve/∪ ω_0 ω_1) (,(and (term (context ω_0)) (term (context ω_1))) ∘ ,(or (term (subject ω_0)) (term (subject ω_1))))])

;; Solve Intersection
;; ------------------
(define-metafunction λCon
  solve/∩ : ω ω -> ω
  [(solve/∩ ω_0 ω_1) (,(or (term (context ω_0)) (term (context ω_1))) ∘ ,(and (term (subject ω_0)) (term (subject ω_1))))])

#|
 ___      _       _   _ _        _   _          
/ __|_  _| |__ __| |_(_) |_ _  _| |_(_)___ _ _  
\__ \ || | '_ (_-<  _| |  _| || |  _| / _ \ ' \ 
|___/\_,_|_.__/__/\__|_|\__|\_,_|\__|_\___/_||_|
                                                
|#

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

;; Blame?
;; ------
(define blame?
  (redex-match? λCon (blame ♭)))

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
  (if (redex-match? λCon M M)
      (car (apply-reduction-relation λCon-reduction (term (· ,M))))
      (error "Invalid λCon-term:" M)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon-->* M)
  (if (redex-match? λCon M M)
      (car (apply-reduction-relation* λCon-reduction (term (· ,M))))
      (error "Invalid λCon-term:" M)))

#|
 _  _     _                 ___             _   _             
| || |___| |_ __  ___ _ _  | __|  _ _ _  __| |_(_)___ _ _  ___
| __ / -_) | '_ \/ -_) '_| | _| || | ' \/ _|  _| / _ \ ' \(_-<
|_||_\___|_| .__/\___|_|   |_| \_,_|_||_\__|\__|_\___/_||_/__/
           |_|                                                

|#

(define
  (λCon-->*/Term M)
  (term (⇓/Term ,(λCon-->* M))))

(define
  (λCon-->*/State M)
  (term (⇓/State ,(λCon-->* M))))

(define
  (λCon-->/Term M)
  (term (⇓/Term ,(λCon--> M))))

(define
  (λCon-->/State M)
  (term (⇓/State ,(λCon--> M))))

(define
  (count ς)
  (term (count/b ,ς)))