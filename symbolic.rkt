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

(define-extended-language λCon-Symbolic λCon
  
  ;; Syntax Extensions
  ;; =================
  
  ;; Immediate Contracts
  ;; -------------------
  ;((I J) .... (I ∩ J))
  
  ;; Contracts
  ((C D) .... ⊤ ⊥)
  
  ;; Delayed Contracts
  ;; -----------------
  ((Q R) .... (C → ⊤) (⊤ → C))
  
  ;; Terms
  ;; -----
  ((L M N) .... (M @ ι C))
  
  
  
  
  
  
  ;; Canonical terms (λJ terms)
  ;; ==========================
  
  ;; Source Terms
  ;; ------------
  ;; Terms without a contract on the outermost position.
  
  ;; Values
  (S0 K (λ x S))
  
  ;; Non-Values
  (S1 x (+blame ♭) (-blame ♭) (S TI) (TI T) (S S) (K T) (op T ...) (if T_0 T_1 T_2))
  
  ;; Source Terms
  (S S0 S1 (S / C))
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  
  ;; Terms with Immediate Contracts/ False
  (TI S1 (TI @ ι I))
  
  ;; Terms with Delayed Contracts
  (TQ S TI T0 T1 (TQ @ ι Q))
  
  ;; Canonical Terms (non-reducable terms)
  (T TQ (T_0 ∥ T_1))
  
  
  
  ;; Reducable terms (non-cannonical terms)
  ;; ======================================
  
  (Reducible
   
   ;; Terms containing a reducable term
   (λ x Reducible) (Reducible M) (M Reducible) (op M ... Reducible N ...) (if M ... Reducible N ...)   (Reducible @ b C)
   
   ;; Optimization
   ;; ------------
   
   ;; Delayed checkes of a delayed contract
   ((λ x M) (M @ ι Q))
   
   ;; Checked of delayed contracts
   ((M @ ι Q) N) 
   
   ;; Imediate contracts in values
   (K @ ι I) ((λ x M) @ ι I)
   
   ;; Contracts on return terms
   (λ x (M @ ι C))
   
   ;; True
   (M @ ι ⊤)
   
   ;; False
   (M @ ι ⊥)
   
   ;; Restructuring
   ;; -------------
   
   ;; Intersection betenn immediate and delayed contract
   (M @ ι (I ∩ C))
   
   ;; Union contracts
   (M @ ι (C ∪ D))
   
   ;; Nested delayed contracts
   ((M @ ι_0 Q) @ ι_1 I)
   
   ;; Top-level assertions
   (M @ ♭ C))
  
  
  
  ;; Contexts
  ;; ========
  
  ;; Baseline Reduction Context
  ;; --------------------------
  ((F G H) hole (λ x F) #|(F T) (M F)|# (F M) (T F) (op T ... F M ...) (if T ... F M ...) (F @ b C)
           (F ∥ N) (T ∥ F)
           (F / C)) ;;  (F M) (T F)
  
  ;; Function Body Context
  ;; ---------------------
  ;; Reduction Context without abstraction.
  (BCtx hole (BCtx M) (T BCtx) (op T ... BCtx T ...) (BCtx @ b C)
        (BCtx ∥ T) (T ∥ BCtx)) ;; (if T ... BCtx T ...)
  
  ;; Assertion Context
  ;; -----------------
  (ACtx hole (ACtx @ ι C)) ;; TODO (X @ ι C) ?
  
  
  
  
  
  ;; Parallel Observations
  (∥ ∩∩ ∪∪)
  
  ;; Traces
  ;  ((Tx Ty) T (Tx / C) (Tx ∥ Ty))
  
  ;; (where (blame ♭) (produce-blame ς)))
  
  
  ;; Trace/ Observation
  ;(O )
  
  ;; TODO observation Context
  ;  (OCtx hole (OCtx ∥ N) (T ∥ OCtx))
  
  
  
  )





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
   
   (--> (ς
         (in-hole F ((in-hole G (any ... (in-hole H (T_l @ ι_1 C)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (T_r @ ι_2 D)) any_r ...)))))
        (ς
         (in-hole F ((in-hole G (any ... (in-hole H ((T_l @ ι_1 C) @ ι_2 D)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H ((T_r @ ι_1 C) @ ι_2 D)) any_r ...)))))
        "Join/Mutual"
        (side-condition
         (and
          (canonical? 
           (term (in-hole F ((in-hole G (any ... (in-hole H (T_l @ ι_1 C)) any_l ...))
                             ∥
                             (in-hole G (any ... (in-hole H (T_r @ ι_2 D)) any_r ...))))))
          (not (eq? (term C) (term D))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (any ... (in-hole H (T @ ι_1 C)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H S) any_r ...)))))
        (ς
         (in-hole F ((in-hole G (any ... (in-hole H (T @ ι_1 C)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (S @ ι_1 C)) any_r ...)))))
        "Join/Left"
        (side-condition
         (canonical? 
          (term (in-hole F ((in-hole G (any ... (in-hole H (T @ ι_1 C)) any_l ...))
                            ∥
                            (in-hole G (any ... (in-hole H S) any_r ...))))))))
   
   (--> (ς
         (in-hole F ((in-hole G (any ... (in-hole H S) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (T @ ι_2 D)) any_r ...)))))
        (ς
         (in-hole F ((in-hole G (any ... (in-hole H (S @ ι_2 D)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (T @ ι_2 D)) any_r ...)))))
        "Join/Right"
        (side-condition
         (canonical? 
          (term (in-hole F ((in-hole G (any ... (in-hole H S) any_l ...))
                            ∥
                            (in-hole G (any ... (in-hole H (T @ ι_2 D)) any_r ...))))))))
   
   
   
   ;; TODO, blame on boh sides
   
   (--> (ς
         (in-hole F ((in-hole G (any ... (in-hole H (blame ♭)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H T) any_r ...)))))
        (ς
         (in-hole F ((in-hole G (any ... (in-hole H (join ∥ (blame ♭) T)) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (join ∥ (blame ♭) T)) any_r ...)))))
        "Join/LeftBlame"
        (side-condition
         (canonical? 
          (term (in-hole F ((in-hole G (any ... (in-hole H (blame ♭)) any_l ...))
                      ∥
                      (in-hole G (any ... (in-hole H T) any_r ...))))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (any ... (in-hole H T) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (blame ♭)) any_r ...)))))
        (ς
         (in-hole F ((in-hole G (any ... (in-hole H (join ∥ T (blame ♭))) any_l ...))
                     ∥
                     (in-hole G (any ... (in-hole H (join ∥ T (blame ♭))) any_r ...)))))
        "Join/RightBlame"
        (side-condition
          (canonical? 
           (term (in-hole F ((in-hole G (any ... (in-hole H T) any_l ...))
                       ∥
                       (in-hole G (any ... (in-hole H (blame ♭)) any_r ...))))))))
   
   (--> (ς
         (in-hole F (T ∥ T))) 
        (ς
         (in-hole F T)) 
        "Join")
   
   
   
   #|   (--> (ς
         (in-hole F ((in-hole G (T @ ι_1 C)) ∥ (in-hole H (T @ ι_2 D)))))
        (ς
         (in-hole F ((in-hole G ((T @ ι_1 C) @ ι_2 D)) ∥ (in-hole H ((T @ ι_1 C) @ ι_2 D)))))
        "Join/LeftRightContract"
        (side-condition
         (and
          (canonical? (term (in-hole F ((in-hole G (T @ ι_1 C)) ∥ (in-hole H (T @ ι_2 D))))))
          (not (eq? (term C) (term D)))
          (term (≈ (in-hole G (T @ ι_1 C)) (in-hole H (T @ ι_2 D)))))))|#
   
   #|(--> (ς
         (in-hole F ((in-hole G (S @ ι C)) ∥ (in-hole H S))))
        (ς
         (in-hole F ((in-hole G (S @ ι C)) ∥ (in-hole H (S @ ι C)))))
        "Join/LeftContract"
        (side-condition 
         (and
          (canonical? (term (in-hole F ((in-hole G (S @ ι C)) ∥ (in-hole H S)))))
          (term (≈ (in-hole G (S @ ι C)) (in-hole H S))))))
   
   (--> (ς
         (in-hole F ((in-hole G S) ∥ (in-hole H (S @ ι C)))))
        (ς
         (in-hole F ((in-hole G (S @ ι C)) ∥ (in-hole H (S @ ι C)))))
        "Join/RightContract"
        (side-condition 
         (and
          (canonical? (term (in-hole F ((in-hole G S) ∥ (in-hole H (S @ ι C))))))
          (term (≈ (in-hole G S) (in-hole G (S @ ι C)))))))|#
   #| 
   (--> (ς
         (in-hole F ((in-hole G (blame ♭)) ∥ (in-hole H T))))
        (ς
         (in-hole F ((in-hole G (join ∥ (blame ♭) T)) ∥ (in-hole H (join ∥ (blame ♭) T)))))
        "Join/LeftBlame"
        (side-condition
         (and
          (canonical? (term (in-hole F ((in-hole H (blame ♭)) ∥ (in-hole H T)))))
          (term (≈ (in-hole G (blame ♭)) (in-hole G T))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (blame ♭)) ∥ (in-hole H T_r))))
        (ς
         (in-hole F ((in-hole G T) ∥ (in-hole H T))))
        "Join/RightBlame"
        (where T (join ∥ T (blame ♭)))
        (side-condition
         (and
          (canonical? (term (in-hole F ((in-hole H (blame ♭)) ∥ (in-hole H T)))))
          (term (≈ (in-hole G (blame ♭)) (in-hole G T_r))))))
   
   
   (--> (ς
         (in-hole F (T ∥ T))) 
        (ς
         (in-hole F T)) 
        "Join") |#
   
   ))
;; use where


;; What if both sides prouce blame ?


;   (--> (ς
;         (in-hole F ((in-hole H (blame ♭)) ∥ (in-hole H T))))
;        (ς
;         (in-hole F (in-hole H (join ∥ (blame ♭) T))))
;        "Join/LeftBlame"
;        (side-condition (canonical? (term (in-hole F ((in-hole H (blame ♭)) ∥ (in-hole H T)))))))

;   (--> (ς
;         (in-hole F ((in-hole H T) ∥ (in-hole H (blame ♭)))))
;        (ς
;         (in-hole F (in-hole H (join ∥ T (blame ♭)))))
;        "Join/RightBlame"
;        (side-condition (canonical? (term (in-hole F ((in-hole H T) ∥ (in-hole H (blame ♭))))))))

;   (--> (ς
;         (in-hole F ((in-hole H T) ∥ (in-hole G (blame ♭)))))
;        (ς
;         (in-hole F (in-hole H (join ∥ T (blame ♭))))) ;; NOT Correct
;        "Join/RightBlame"
;        (side-condition 
;         (and
;          (canonical? (term (in-hole F ((in-hole H T) ∥ (in-hole H (blame ♭))))))
;          (term (≈ (in-hole H T) (in-hole G (blame ♭))))
;          )))  

;   (--> (ς
;         (in-hole F ((in-hole H (T @ ι C)) ∥ (in-hole H S))))
;        (ς
;         (in-hole F ((in-hole H (T @ ι C)) ∥ (in-hole H (S @ ι C)))))
;        "Join/LeftContract"
;        (side-condition (canonical? (term (in-hole F ((in-hole H (T @ ι C)) ∥ (in-hole H S)))))))
;   
;   (--> (ς
;         (in-hole F ((in-hole H S) ∥ (in-hole H (T @ ι C)))))
;        (ς
;         (in-hole F ((in-hole H (S @ ι C)) ∥ (in-hole H (T @ ι C)))))
;        "Join/RightContract"
;        (side-condition (canonical? (term (in-hole F ((in-hole H S) ∥ (in-hole H (T @ ι C))))))))

;   (--> (ς
;         (in-hole F ((in-hole H (T_1 @ ι_1 C)) ∥ (in-hole H (T_2 @ ι_2 D)))))
;        (ς
;         (in-hole F ((in-hole H ((T_1 @ ι_1 C) @ ι_2 D)) ∥ (in-hole H ((T_2 @ ι_1 C) @ ι_2 D)))))
;        "Join/LeftRightContract"
;        (side-condition
;         (and
;          (canonical? (term (in-hole F ((in-hole H (T_1 @ ι_1 C)) ∥ (in-hole H (T_2 @ ι_2 D))))))
;          (not (eq? (term C) (term D))))))





;   (--> (ς
;         (in-hole F ((in-hole H (T @ ι_1 C)) ∥ (in-hole G (T @ ι_2 D)))))
;        (ς
;         (in-hole F ((in-hole H ((T @ ι_1 C) @ ι_2 D)) ∥ (in-hole G ((T @ ι_1 C) @ ι_2 D)))))
;        "Join/LeftRightContract2"
;        (side-condition
;         (and
;          (canonical? (term (in-hole F ((in-hole H (T @ ι_1 C)) ∥ (in-hole G (T @ ι_2 D))))))
;          (not (eq? (term C) (term D)))
;          (term (≈ (in-hole H (T @ ι_1 C)) (in-hole G (T @ ι_2 D))))
;          )))


#|
      (--> (ς
         (in-hole F 
                  ((in-hole H (T_1 (T_11 @ ι_1 C)))
                   ∥
                   (in-hole H (T_2 (T_22 @ ι_2 D)))
                   
                   )))
        (ς
         (in-hole F ((in-hole H (T_1 ((T_11 @ ι_1 C) @ ι_2 D))) ∥ (in-hole H (T_2 ((T_22 @ ι_1 C) @ ι_2 D))))))
        "Join/App"
        
        (side-condition (and (canonical? (term (in-hole H (T_1 (T_11 @ ι_1 C)))))
                             (canonical? (term (in-hole H (T_2 (T_22 @ ι_2 D)))))
                             (not (eq? (term C) (term D)))
                             ))
        )
   
   
   
   
   |#









;; TODO, will this work 




;   (--> (ς
;         (in-hole F ((in-hole H (T_1 (T_11 @ ι_1 C))) ∥ (in-hole H (T_2 (T_22 @ ι_2 D))))))
;        (ς
;         (in-hole F ((in-hole H (T_1 ((T_11 @ ι_1 C) @ ι_2 D))) ∥ (in-hole H (T_2 ((T_22 @ ι_1 C) @ ;ι_2 D))))))
;        "Join/App"
;        
;        (side-condition (and (canonical? (term (in-hole H (T_1 (T_11 @ ι_1 C)))))
;                             (canonical? (term (in-hole H (T_2 (T_22 @ ι_2 D)))))
;                             (not (eq? (term C) (term D)))
;                             ))
;        )




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



(define-metafunction λCon-Symbolic
  join : ∥ M M -> M
  ;; intersection/ negative blame
  [(join ∩∩ (-blame ♭) T) T]
  [(join ∩∩ T (-blame ♭)) T]
  ;; intersection/ positive blame
  [(join ∩∩ (+blame ♭) T) (+blame ♭)]
  [(join ∩∩ T (+blame ♭)) (+blame ♭)]
  ;; union/ negative blame
  [(join ∪∪ (-blame ♭) T) (-blame ♭)]
  [(join ∪∪ T (-blame ♭)) (-blame ♭)]
  ;; union/ positive blame
  [(join ∪∪ (+blame ♭) T) T]
  [(join ∪∪ T (+blame ♭)) T])



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


;; Term of (⇓/Term)
;; ----------------
(define-metafunction λCon
  termOf : (ς T) -> T
  [(termOf (ς T)) T])

;; State of (⇓/State)
;; ------------------
(define-metafunction λCon
  stateOf : (ς T) -> ς
  [(stateOf (ς T)) ς])


;; Canonical? (non-reducable terms)
;; --------------------------------
(define canonical?
  (redex-match? λCon-Symbolic T))

;; Reducible? (non-canonical terms)
;; --------------------------------
(define reducible? 
  (redex-match? λCon-Symbolic Reducible))


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

