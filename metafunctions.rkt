#lang racket


  
  (define-metafunction λCon
    lookup : named -> (flat M)
    
    [(lookup ⊤) (flat (λ x #t))]
    [(lookup ⊥) (flat (λ x #f))]
    
    [(lookup Num?) (flat (λ x (number? x)))]
    [(lookup Str?) (flat (λ x (string? x)))]
    [(lookup Bool?) (flat (λ x (boolean? x)))]
    
    [(lookup Pos?) (flat (λ x (> x 0)))]
    [(lookup Nat?) (flat (λ x (or (> x 0) (= x 0))))]
    [(lookup Neg?) (flat (λ x (< x 0)))]
    ) 
  
  (define-metafunction λCon
    pretty : (flat M) -> I
    
    [(pretty (flat (λ x #t))) ⊤]
    [(pretty (flat (λ x #f))) ⊥]
    
    [(pretty (flat (λ x (number? x)))) Num?]
    [(pretty (flat (λ x (string? x)))) Str?]
    [(pretty (flat (λ x (boolean? x)))) Bool?]
    
    [(pretty (flat (λ x (> x 0)))) Pos?]
    [(pretty (flat (λ x (or (> x 0) (= x 0))))) Nat?]
    [(pretty (flat (λ x (< x 0)))) Neg?]
    
    ;; Default Case
    [(pretty any) any]
    ) 
  


  
  ;; for subset realtion
  
  
  
  
  
  
  
  ;; Predicate Subset
  ;; ================
  ;(< #t #f) defined on real
  ;(number? +nan.0)
  ;(real? +nan.0)
  
  
  
  
  ;(define-metafunction λCon
  ;  ∈ : P (P_0 ...) -> boolean
  ;  [(∈ P (P_0 P_1 ...)) ,(or (term (≤ P P_0)) (term (∈ P (P_1 ...))))]
  ;  [(∈ P ()) #f])
  
  ;(define-metafunction λCon
  ;  ≤/ : (P ...) (P ...) -> boolean
  ;  [(≤/ (P_0 P_1 ...) (P ...)) ,(and (term (∈ P_0 (P ...))) (term (≤/ (P_1 ...) (P ...))))]
  ;  [(≤/ () (P ...)) #t]
  ;  )
  
  ;(define-metafunction λCon
  ;  ⊑/flat : (flat P ...) (flat P ...) -> boolean
  ;  [(⊑/flat (flat P_0 ...) (flat P_1 ...)) (≤ (P_0 ...) (P_0 ...))]
  ;  )
  
  
  
  
  
  ;; Naive Subsets of Contracts (⊆)
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
    [(⊑/context (C_0 ∩ D_0) C_1) ,(and (term (⊑/context C_0 C_1)) (term (⊑/context D_0 C_1)))]
    [(⊑/context C_0 (C_1 ∩ D_1)) ,(or (term (⊑/context C_0 C_1)) (term (⊑/context C_0 D_1)))]
    ;; Union Contract
    [(⊑/context (C_0 ∪ D_0) C_1) ,(or (term (⊑/context C_0 C_1)) (term (⊑/context D_0 C_1)))]
    [(⊑/context C_0 (C_1 ∪ D_1)) ,(and (term (⊑/context C_0 C_1)) (term (⊑/context C_0 D_1)))]
    ;; If not otherwise mentioned
    [(⊑/context any ...) #f]
    )
  
  (define-metafunction λCon
    ⊑/subject : C D -> boolean
    ;; Flat Contracts
    [(⊑/subject (flat M) (flat M)) #t]
    [(⊑/subject (flat M) (flat N)) #f]
    ;; Predefined Contracts
    [(⊑/subject P_0 P_1) (≤ P_0 P_1)]
    
    ;; TODO
    ;[(⊑/subject named ⊤) #t]
    ;[(⊑/subject ⊥ named) #t]
    ;[(⊑/subject named named) #t]
    ;[(⊑/subject named named) #t]
    ;[(⊑/subject Nat? Num?) #t]
    ;[(⊑/subject Pos? Nat?) #t]
    ;[(⊑/subject Pos? Num?) #t]
    ;[(⊑/subject named_0 named_1) #f]
    
    ;; Abstraction
    [(⊑/subject (Λ x C) (Λ x D)) (⊑/subject C D)]
    ;; Function Contract
    [(⊑/subject (C_0 → D_0) (C_1 → D_1)) ,(and (term (⊑/context C_0 C_1)) (implies (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 D_1))))]
    ;; Dependent Contract
    [(⊑/subject (x → A_0) (x → A_1)) (⊑/subject A_0 A_1)]
    ;; Intersection Contract
    [(⊑/subject (C_0 ∩ D_0) C_1) ,(or (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 C_1)))]
    [(⊑/subject C_0 (C_1 ∩ D_1)) ,(and (term (⊑/subject C_0 C_1)) (term (⊑/subject C_0 D_1)))]
    ;; Union Contract
    [(⊑/subject (C_0 ∪ D_0) C_1) ,(and (term (⊑/subject C_0 C_1)) (term (⊑/subject D_0 C_1)))]
    [(⊑/subject C_0 (C_1 ∪ D_1)) ,(or (term (⊑/subject C_0 C_1)) (term (⊑/subject C_0 D_1)))]
    ;; If not otherwise mentioned
    [(⊑/subject any ...) #f]
    )
  
  ;; Predicate (Refinement) Subset (≤)
  ;; ---------------------------------
  ;; Returns true if the left predicate is subset or equals to the reight predicate, 
  ;; false otherwise.
  
  (define-metafunction λCon
    ≤ : P P -> boolean
    ;; Base cases
    [(≤ P P) #t]
    [(≤ P ⊤) #t]
    [(≤ ⊥ P) #t]
    ;; Chain Lookup  
    [(≤ (P_M / M) (P_N / N)) ,(or
                               (term (≤ P_M (P_N / N)))
                               (and
                                (term (≤ P_M P_N))
                                (term (≼ M N))))]  
    ;; Unroll predefined predicates
    [(≤ predefined P) (≤ (lookup/ predefined) P)]
    [(≤ P predefined) (≤ P (lookup/ predefined))]
    ;; End
    [(≤ any ...) #f])





  ;; Predicate (Refinement) Subset (≤)
  ;; ---------------------------------
  ;; Returns true if the left predicate is subset or equals to the reight predicate, 
  ;; false otherwise.
  
  (define-metafunction λCon
    ≤ : P P -> boolean
    ;; Base cases
    [(≤ P P) #t]
    [(≤ P ⊤) #t]
    [(≤ ⊥ P) #t]
    ;; Chain Lookup  
    [(≤ (P_M / M) (P_N / N)) ,(or
                               (term (≤ P_M (P_N / N)))
                               (and
                                (term (≤ P_M P_N))
                                (term (≼ M N))))]  
    ;; Unroll predefined predicates
    [(≤ predefined P) (≤ (lookup/ predefined) P)]
    [(≤ P predefined) (≤ P (lookup/ predefined))]
    ;; End
    [(≤ any ...) #f])







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








