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








