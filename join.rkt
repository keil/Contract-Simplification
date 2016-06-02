#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "subset.rkt")

(provide (all-defined-out))

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
   λCon-Subset
   #:domain (ς any)
   
   (--> (ς
         (in-hole F (T ∥ T))) 
        (ς
         (in-hole F T)) 
        "Join")
   
   (--> (ς
         (in-hole F ((in-hole H (in-hole ACtx_l S_l))
                     ∥
                     (in-hole H (in-hole ACtx_r S_r)))))
        (ς
         (in-hole F ((in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_l S_r)))
                     ∥
                     (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_r S_l))))))
        "Join/Term"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole H (in-hole ACtx_l S_l))
                            ∥
                            (in-hole H (in-hole ACtx_r S_r))))))))
   
   (--> (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_l S_r))) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_r S_l))) T_r ... )))))
        "Join/Op"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole G (op T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                            ∥
                            (in-hole G (op T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... ))))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_l S_r))) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_r S_l))) T_r ... )))))
        "Join/If"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole G (if T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                            ∥
                            (in-hole G (if T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... ))))))))
   
   
   (--> (ς
         (in-hole F ((in-hole G (T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_l S_r))) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) (⊔/Term S_r S_l))) T_r ... )))))
        "Join/App"
        (side-condition (not (term (≈ (in-hole ACtx_l S_l) (in-hole ACtx_r S_r)))))
        (side-condition 
         (canonical? 
          (term (in-hole F ((in-hole G (T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                            ∥
                            (in-hole G (T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... ))))))))
   
   
   
   ))

;; merge
(define-metafunction λCon-Subset
  ⊕/ACtx : ACtx ACtx -> ACtx
  [(⊕/ACtx ACtx_l ACtx_r) (in-hole ACtx_l (\\ ACtx_r ACtx_l))])

(define-metafunction λCon-Subset
  ∈/ACtx : C ACtx -> boolean
  [(∈/ACtx C (ACtx @ ι C)) #t]
  [(∈/ACtx C (ACtx @ ι D)) (∈/ACtx C ACtx)]
  [(∈/ACtx C hole) #f])  

;; context set minus
(define-metafunction λCon-Subset
  \\ : : ACtx ACtx -> ACtx
  [(\\ (ACtx_0 @ ι C) ACtx) (\\ ACtx_0 ACtx) (side-condition (term (∈/ACtx C ACtx)))]
  [(\\ (ACtx_0 @ ι C) ACtx) ((\\ ACtx_0 ACtx) @ ι C) (side-condition (not (term (∈/ACtx C ACtx))))]
  [(\\ hole ACtx) hole])

(define-metafunction λCon-Subset
  ≡/ACtx : ACtx ACtx -> boolean
  [(≡/ACtx (ACtx_0 @ ι C) (ACtx_1 @ ι C)) (≡/ACtx ACtx_0 ACtx_1)]
  [(≡/ACtx hole hole) #t] 
  [(≡/ACtx any ...) #f])


;; TODO, maybe we need a one level equivalence of contetx


(define-metafunction λCon-Subset
  ⊔/Term : T T -> T
  [(⊔/Term (blame ♭) T) T]
  [(⊔/Term T (blame ♭)) T]
  [(⊔/Term T T) T])



#|

(define-metafunction λCon-Baseline
  ≈ : T T -> boolean
  ;; equals terms
  [(≈ T T) #t]
  ;; equal contracts on differnt terms
  [(≈ (T_l @ ι C) (T_r @ ι C)) #t]
  ;; one side blame
  [(≈ (blame ♭) T) #f]
  [(≈ T (blame ♭)) #f]
  ;; differnt terms without contract
  [(≈ S_l S_r) #t]
  ;; othweise
  [(≈ any ...) #f])



(define-metafunction λCon-Baseline
  √ : ∥ T T -> T
  ;; intersection/ negative blame
  [(√ (∩∩ b) (-blame ♭) T) T]
  [(√ (∩∩ b) T (-blame ♭)) T]
  ;; intersection/ positive blame
  [(√ (∩∩ b) (+blame ♭) T) (+blame ♭)]
  [(√ (∩∩ b) T (+blame ♭)) (+blame ♭)]
  ;; union/ negative blame
  [(√ (∪∪ b) (-blame ♭) T) (-blame ♭)]
  [(√ (∪∪ b) T (-blame ♭)) (-blame ♭)]
  ;; union/ positive blame
  [(√ (∪∪ b) (+blame ♭) T) T]
  [(√ (∪∪ b) T (+blame ♭)) T]
  
  ;; XXX
  [(√ (∩∩ b) T S) T]
  [(√ (∪∪ b) T S) T]) ;; TODO

|#


;; TODO, use join with (∩∩ ♭)

#|
  (define-metafunction λCon-Baseline
    ⊔/x : T T -> T
    ;; intersection/ negative blame
    [(⊔/x ∩∩ (-blame ♭) T) T]
    [(⊔/x ∩∩ T (-blame ♭)) T]
    ;; intersection/ positive blame
    [(⊔ ∩∩ (+blame ♭) T) (+blame ♭)]
    [(⊔ ∩∩ T (+blame ♭)) (+blame ♭)]
    ;; union/ negative blame
    [(⊔ ∪∪ (-blame ♭) T) (-blame ♭)]
    [(⊔ ∪∪ T (-blame ♭)) (-blame ♭)]
    ;; union/ positive blame
    [(⊔ ∪∪ (+blame ♭) T) T]
    [(⊔ ∪∪ T (+blame ♭)) T])
 |# 








#|
    _     _      
 _ | |___(_)_ _  
| || / _ \ | ' \ 
 \__/\___/_|_||_|
                 
|#
;; TODO, change join function.
;; A context/subject blame may only be removed if it is a blame term from the splitted contract.
;; Thus, say ∥ = (∪∪ ♭).

#|

(define-metafunction λCon-Baseline
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

;; Join Reduction (λCon/Join-->)
;; -----------------------------
(define
  (λCon/Join~~> ς configuration)
  (if (redex-match? λCon-Baseline (ς T) configuration)
      (car (apply-reduction-relation Join-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; Join Reduction (λCon/Join-->*)
;; ------------------------------
(define
  (λCon/Join~~>* configuration)
  (if (redex-match? λCon-Baseline (ς T) configuration)
      (car (apply-reduction-relation* Join-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))