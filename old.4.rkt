#lang racket

#|
;; Naive Subtyping
(define-metafunction λCon
  ⊑ : C D -> boolean
  
  ;; Flat Contracts
  [(⊑ (flat M) (flat M)) #t]
  [(⊑ (flat M) (flat N)) #f]
  
  ;; TODO
  [(⊑ named ⊤) #t]
  [(⊑ ⊥ named) #t]
  [(⊑ named named) #t]
  [(⊑ named named) #t]
  [(⊑ Nat? Num?) #t]
  [(⊑ Pos? Nat?) #t]
  [(⊑ Pos? Num?) #t]
  [(⊑ named_0 named_1) #f]
  
  ;; Function Contract
  [(⊑ (C_0 → D_0) (C_1 → D_1)) ,(and (term (⊑ C_0 C_1)) (term (⊑ D_0 D_1)))]

  ;; Intersection
  [(⊑ (C_0 ∩ D_0) C_1) ,(and (term (⊑ C_0 C_1)) (term (⊑ D_0 C_1)))]
  [(⊑ C_0 (C_1 ∩ D_1)) ,(or (term (⊑ C_0 C_1)) (term (⊑ C_0 D_1)))]
  
  ;; Union
  [(⊑ (C_0 ∪ D_0) C_1) ,(or (term (⊑ C_0 C_1)) (term (⊑ D_0 C_1)))]
  [(⊑ C_0 (C_1 ∪ D_1)) ,(and (term (⊑ C_0 C_1)) (term (⊑ C_0 D_1)))]
  ;; Dependent
  ;; TODO  
) 
|#