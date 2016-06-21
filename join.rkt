#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "subset.rkt")

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

(define-extended-language λCon-Join λCon-Subset
  
  ;; Source Terms
  ;; ------------
  ;; Terms without a contract on the outermost position.
  (S K x (λ x ... T) (T T) (op T ...) (if T ...) (blame ♭))
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  (T S (T @ ι C))
  
  ;; Trace
  ;; -----
  ;; Parallel traces.
  (Trace T (Trace_l ∥ Trace_r))
  
  ;; Traces
  ;; ------
  ;; Trace context.
  (Traces hole (Traces ∥ Trace) (M ∥ Traces))
  
  ;; Join Context
  ;; ------------
  ((F G H)
   hole (λ x ... F) (F M) (M F) (op M ... F N ...) (if M ... F N ...) (F0 @ b C))
  ((F0 G0 H0) 
   (λ x ... F) (F M) (M F) (op M ... F N ...) (if M ... F N ...) (F0 @ b C)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Join-reduction
;; ==================
;; Join splitted observations.

(define Join-reduction
  (reduction-relation
   λCon-Join
   #:domain (ς any)
   
   (--> (ς
         (in-hole Traces (M ∥ M))) 
        (ς
         (in-hole Traces M)) 
        "Join")
   
   (--> (ς
         (in-hole Traces ((in-hole G (in-hole ACtx_l S))
                          ∥
                          (in-hole H (in-hole ACtx_r S)))))
        (ς
         (in-hole Traces ((in-hole G (in-hole (⊔/ACtx ACtx_l ACtx_r) S))
                          ∥
                          (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S)))))
        "Join/Context"
        (side-condition (term (≡/Ctx G H)))
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r)))))
   
   (--> (ς
         (in-hole Traces ((in-hole G (in-hole ACtx_l S_l))
                          ∥
                          (in-hole H (in-hole ACtx_r S_r)))))
        (ς
         (in-hole Traces ((in-hole G (in-hole ACtx_l (⊔/Term S_l S_r)))
                          ∥
                          (in-hole H (in-hole ACtx_r (⊔/Term S_r S_l))))))
        "Join/Term"
        (side-condition (term (≡/Ctx G H)))
        (side-condition 
         (or 
          (redex-match? λCon-Subset (blame ♭) (term S_l))
          (redex-match? λCon-Subset (blame ♭) (term S_r)))))
   ))

#|
 ___           _          _             _     
| __|__ _ _  _(_)_ ____ _| |___ _ _  __| |___ 
| _|/ _` | || | \ V / _` | / -_) ' \/ _` / -_)
|___\__, |\_,_|_|\_/\__,_|_\___|_||_\__,_\___|
       |_|                                    

|#

;; Context equivalende
;; -------------------

(define-metafunction λCon-Join
  ≡/Ctx : G H -> boolean
  [(≡/Ctx any any) #t]
  [(≡/Ctx (λ x ... G) (λ x ... H)) (≡/Ctx G H)]
  [(≡/Ctx (op T ... G M_n ...) (op T ... H N_n ...)) ,(and
                                                       (term (≡/Ctx G H))
                                                       (term (≡/Terms (M_n ...) (N_n ...))))]
  [(≡/Ctx (if T ... G M_n ...) (op T ... H N_n ...)) ,(and
                                                       (term (≡/Ctx G H))
                                                       (term (≡/Terms (M_n ...) (N_n ...))))]
  [(≡/Ctx (G M) (H N)) ,(and (term (≡/Ctx G H)) (term (≡/Term M N)))]
  [(≡/Ctx (T G) (T H)) ,(term (≡/Ctx G H))]
  [(≡/Ctx (G @ ι C) H) (≡/Ctx G H)]
  [(≡/Ctx G (H @ ι C)) (≡/Ctx G H)]  
  [(≡/Ctx any ... ) #f])

;; Term set equivalende
;; --------------------

(define-metafunction λCon-Join
  ≡/Terms : (M ...) (N ...) -> boolean
  [(≡/Terms any any) #t]
  [(≡/Terms (M_0 M_n ...) (N_0 N_n ...)) ,(and
                                           (term (≡/Term M_0 N_0))
                                           (term (≡/Terms (M_n ...) (N_n ...))))]
  [(≡/Terms any ... ) #f])

;; Term equivalende
;; ----------------

(define-metafunction λCon-Join
  ≡/Term : M N -> boolean
  [(≡/Term any any) #t]
  [(≡/Term (blame ♭) N) #t]
  [(≡/Term M (blame ♭)) #t]
  [(≡/Term (λ x ... M) (λ x ... N)) (≡/Term M N)]
  [(≡/Term (op M_0 M_n ...) (op N_0 N_n ...)) ,(and
                                                (term (≡/Term M_0 N_0))
                                                (term (≡/Terms (M_n ...) (N_n ...))))]  
  [(≡/Term (if M_0 M_n ...) (if N_0 N_n ...)) ,(and
                                                (term (≡/Term M_0 N_0))
                                                (term (≡/Terms (M_n ...) (N_n ...))))]  
  [(≡/Term (M_0 M_1) (N_0 N_1)) ,(and (term (≡/Term M_0 N_0)) (term (≡/Term M_1 N_1)))]
  [(≡/Term (M @ ι C) N) (≡/Term M N)]
  [(≡/Term M (N @ ι C)) (≡/Term M N)]
  [(≡/Term any ... ) #f])


;; Assertion Context Equivalence
;; -----------------------------
(define-metafunction λCon-Subset
  ≡/ACtx : ACtx ACtx -> boolean
  [(≡/ACtx (ACtx_0 @ ι C) (ACtx_1 @ ι C)) (≡/ACtx ACtx_0 ACtx_1)]
  [(≡/ACtx hole hole) #t] 
  [(≡/ACtx any ...) #f])

#|
    _     _      
 _ | |___(_)_ _  
| || / _ \ | ' \ 
 \__/\___/_|_||_|
                 
|#

;; Join Assertion Context
;; ----------------------
(define-metafunction λCon-Subset
  ⊔/ACtx : ACtx ACtx -> ACtx
  [(⊔/ACtx ACtx_l ACtx_r) (in-hole ACtx_l (\\ ACtx_r ACtx_l))])
;;

;; Context Minus
;; -------------
(define-metafunction λCon-Subset
  \\ : ACtx ACtx -> ACtx
  [(\\ (ACtx_0 @ ι C) ACtx) (\\ ACtx_0 ACtx) (side-condition (term (∈/ACtx C ACtx)))]
  [(\\ (ACtx_0 @ ι C) ACtx) ((\\ ACtx_0 ACtx) @ ι C) (side-condition (not (term (∈/ACtx C ACtx))))]
  [(\\ hole hole) hole]
  [(\\ hole ACtx) hole])

;; Contract In Context
;; -------------------
(define-metafunction λCon-Subset
  ∈/ACtx : C ACtx -> boolean
  [(∈/ACtx C (ACtx @ ι C)) #t]
  [(∈/ACtx C (ACtx @ ι D)) (∈/ACtx C ACtx)]
  [(∈/ACtx C hole) #f])  

;; Join Term
;; ---------
(define-metafunction λCon-Subset
  ⊔/Term : T T -> T
  [(⊔/Term (blame ♭) T) T]
  [(⊔/Term T (blame ♭)) T])

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

;; Join Reduction (λCon/Join-->)
;; -----------------------------
(define
  (λCon/Join~~> ς configuration)
  (if (redex-match? λCon-Join (ς Trace) configuration)
      (car (apply-reduction-relation Join-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; Join Reduction (λCon/Join-->*)
;; ------------------------------
(define
  (λCon/Join~~>* configuration)
  (if (redex-match? λCon-Join (ς Trace) configuration)
      (car (apply-reduction-relation* Join-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))