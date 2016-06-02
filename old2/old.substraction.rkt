#lang racket
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


#|
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

|#