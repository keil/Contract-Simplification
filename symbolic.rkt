


#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Baseline-reduction
;; ==================
;; Verifies all (immediate) contracts that can be check at compile time
;; and unrolls all contracts.

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic
   #:domain (ς any)
   

   
   ))
   
   
   





(define-metafunction λCon-Symbolic
  ≈ : any any -> boolean
  ;; Base Case
  [(≈ any any) #t]
  ;; Blame
  [(≈ (blame ♭) any) #t]
  [(≈ any (blame ♭)) #t]
  ;; Terms with Contract
  [(≈ (any_0 @ b C) any_1) (≈ any_0 any_1)]
  [(≈ any_0 (any_1 @ b C)) (≈ any_0 any_1)]
  ;; Term Deconstruction
  [(≈ (any_0 any_i ...) (any_1 any_j ...)) ,(and (term (≈ any_0 any_1)) (term (≈ (any_i ...) (any_j ...))))] 
  ;; Otherwise
  [(≈ any_0 any_1) #f])






#|
;; Finalize-reduction
;; ==================


;(define Finalize-reduction
;(;extend-reduction-relation
;Subset-reduction
;λCon-Symbolic
;#:domain (ς any)

(define Finalize-reduction
  (reduction-relation
   λCon-Symbolic
   #:domain (ς any)
   
   
   ;; TODO, update blame state that fail information did nt get lost
   ;; in terms of a function contract
   
   (--> (ς
         (in-hole F ((in-hole H (blame ♭)) ∥ (in-hole H T))))
        (ς
         (in-hole F (in-hole H (join ∥ (blame ♭) T))))
        "Join/LeftBlame"
        (side-condition (and (canonical? (term (in-hole H (blame ♭))))
                             (canonical? (term (in-hole H T)))))
        )
   
   ;   (--> (ς
   ;         (in-hole F ((in-hole H (+blame ♭)) ∩∩ (in-hole H T))))
   ;        (ς
   ;         (in-hole F (in-hole H (+blame ♭))))
   ;        "Join/LeftPositiveBlame"
   ;        (side-condition (and (canonical? (term (in-hole H (+blame ♭))))
   ;                             (canonical? (term (in-hole H T)))))
   ;        );;;
   
   (--> (ς
         (in-hole F ((in-hole H T) ∥ (in-hole H (blame ♭)))))
        (ς
         (in-hole F (in-hole H (join ∥ T (blame ♭)))))
        "Join/RightBlame"
        (side-condition (and (canonical? (term (in-hole H (blame ♭))))
                             (canonical? (term (in-hole H T)))))
        )
   
   
   
   
   
   (--> (ς
         (in-hole F ((in-hole H (T @ ι C)) ∥ (in-hole H S))))
        (ς
         (in-hole F ((in-hole H (T @ ι C)) ∥ (in-hole H (S @ ι C)))))
        "Join/LeftContract"
        (side-condition (and (canonical? (term (in-hole H (T @ ι C))))
                             (canonical? (term (in-hole H S)))))
        )
   
   (--> (ς
         (in-hole F ((in-hole H S) ∥ (in-hole H (T @ ι C)))))
        (ς
         (in-hole F ((in-hole H (S @ ι C)) ∥ (in-hole H (T @ ι C)))))
        "Join/RightContract"
        
        (side-condition (and (canonical? (term (in-hole H S)))
                             (canonical? (term (in-hole H (T @ ι C))))))
        ) 
   
   (--> (ς
         (in-hole F ((in-hole H (T_1 @ ι_1 C)) ∥ (in-hole H (T_2 @ ι_2 D)))))
        (ς
         (in-hole F ((in-hole H ((T_1 @ ι_1 C) @ ι_2 D)) ∥ (in-hole H ((T_2 @ ι_1 C) @ ι_2 D)))))
        "Join/LeftRightContract"
        
        (side-condition (and (canonical? (term (in-hole H (T_1 @ ι_1 C))))
                             (canonical? (term (in-hole H (T_2 @ ι_2 D))))
                             (not (eq? (term C) (term D)))
                             ))
        )
   
   
   (--> (ς
         (in-hole F ((in-hole H (T_1 (T_11 @ ι_1 C))) ∥ (in-hole H (T_2 (T_22 @ ι_2 D))))))
        (ς
         (in-hole F ((in-hole H (T_1 ((T_11 @ ι_1 C) @ ι_2 D))) ∥ (in-hole H (T_2 ((T_22 @ ι_1 C) @ ι_2 D))))))
        "Join/App"
        
        (side-condition (and (canonical? (term (in-hole H (T_1 (T_11 @ ι_1 C)))))
                             (canonical? (term (in-hole H (T_2 (T_22 @ ι_2 D)))))
                             (not (eq? (term C) (term D)))
                             ))
        )
   
   (--> (ς
         (in-hole F (T ∥ T))) 
        (ς
         (in-hole F T)) 
        "Join")
   
   ))


|#




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




#|
;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Finalize~~> ς T)
  (if (redex-match? λCon-Symbolic Tx T)
      (car (apply-reduction-relation Finalize-reduction (term (,ς ,T))))
      (error "Invalid λCon-term:" T)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Finalize~~>* configuration)
  (if (redex-match? λCon-Symbolic (ς Tx) configuration)
      (car (apply-reduction-relation* Finalize-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

|#