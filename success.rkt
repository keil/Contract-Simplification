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

(define-extended-language λCon-Success λCon-Subset
  
  ;; Canonical terms (λJ terms)
  ;; ==========================
  
  ;; Source Terms
  ;; ------------
  ;; Terms without a contract on the outermost position.
  
  ;; Values
  (SVal
   K (side-condition
      (name _fundec (λ x ... S))
      (not
       (redex-match? λCon-Subset (λ x ... y z ... (in-hole G (y @ ι I))) (term _fundec))
       ;(redex-match? λCon-Subset (λ x ... y z ... (in-hole G ((y @ ι I) T ...))) (term _fundec))
       ;; Ti x
       ;; Not correct, because x must be free in S, correct ist term S ist not canonical if so
       )
      ))
  
  ;; Non-Values
  (SNonVal
   (blame ♭) ;; TODO
   (TIx TQx ...) (TCons TQx ...) (TAbs TI ...) (TAbs TVal ...)
   (op TQx ...) (if TQx_0 TQx_1 TQx_2))
  
  ;; Source Terms
  (S SVal SNonVal x)
  
  ;; Terms
  ;; -----
  ;; Terms with non-reducable contracts.
  
  ;; Terms with Immediate Contracts/ False
  (TI SNonVal
      (SNonVal @ ι I)
      (side-condition 
       ((in-hole VCtx (TI @ ι_i (name _I I))) @ ι_r (name _J J))
       (not (or (term (⊑/naive _I _J)) (term (⊑/naive _J _I))))))
  
  (TIx TI x)
  
  ;; TODO. is it correct to have VCtx here?
  ;; dont think so, Actx must be correct , boy it shoudl only contain immediate contract
  ;; same for TQ
  
  ;; Terms with Delayed Contracts
  (TQ SVal TI 
      (SVal @ ι Q) (TI @ ι Q)
      (side-condition 
       ((in-hole ACtx (TQ @ ι_q (name _Q Q))) @ ι_r (name _R R))
       (not (or (term (⊑/naive _Q _R)) (term (⊑/naive _R _Q))
                
                ;; TODO, must be part of the same blame
                ;; replace all (T @ ι C) by (T @ ♭ ι C)
                (and 
                 (or (term (⊑/ordinary _Q _R)) (term (⊑/ordinary _R _Q)))
                 ;(equal? (term (blame-of ι_r ς)) (term (blame-of ι_r ς)))
                 )
                
                ))))
  
  (TQx TQ x)
  
  ;; Canonical Terms (non-reducable terms)
  (T TQx (T_0 ∥ T_1) ((blame ♭) @ ι ⊥)) ;; TODO
  
  
  (X hole (op T ... X M ...) (if T ... X M ...) (SNonVal X))
  
  )

;  (term ((λ f (λ x (f f x))) ((λ f x (if (= x 1) 1 (* x (f f (- x 1))))) @ ♭ (⊤ Natural? → Positive?)))))
;(redex-match λCon-Success (in-hole F (T @ ♭ C)) (term ((λ f (λ x (f f x))) ((λ f x (if (= x 1) 1 (* x (f f (- x 1))))) @ ♭ (⊤ Natural? → Positive?)))))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Lift (one level)
;; ================
;; Verifies all (immediate) contracts that can be check at compile time
;; and unroll all intersection/union contracts.

(define Success-reduction
  (extend-reduction-relation
   Subset-reduction
   λCon-Success
   #:domain (ς any)
   
   ;; Multilevel Lift (up) Contract
   ;; -----------------------------
   ;; Rule [Lift] lifts an contract contract C
   ;; on argument x and creates a new function contract.
   
   (--> (ς
         (in-hole F (λ x ... y z ... (in-hole G (y @ ♭ ι I)))))
        (((ι ◃ (¬ ι1)) ς)
         (in-hole F ((λ x ... y z ... (in-hole G y)) @ ♭ ι1 (build (x ⊤) ... (y I) (z ⊤) ... ⊤)))) 
        "Lift"
        (fresh ι1))
   
   (--> (ς
         (in-hole F (λ x ... y z ... (in-hole X (y @ ♭ ι Q)))))
        (((ι ◃ (¬ ι1)) ς)
         (in-hole F ((λ x ... y z ... (in-hole X y)) @ ♭ ι1 (build (x ⊤) ... (y Q) (z ⊤) ... ⊤))))
        "Lift/Q"
        (fresh ι1)
        ;(side-condition (canonical?/Subset (term (λ x ... y z ... (in-hole G (y @ ι C))))))
        )
   
   (--> (ς
         (in-hole F (T @ ι ⊥)))
        (((ι ◃ (τ #f)) ς)
         (in-hole F T))
        "Blame")
   
   (--> (ς
         (in-hole F (in-hole ACtx ((T @ ι_0 (⊤ → D)) @ ι_1 (C → ⊤)))))
        (ς
         (in-hole F (in-hole ACtx (T @ ♭1 (C → D)))))
        "Condense"
        ;(side-condition (not (equal? (term (blame-of ι_0 ς)) (term (blame-of ι_1 ς)))))
        ;; Side condition, i_o must be negative
        ;(fresh ι2)
        (fresh ♭1))
   
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

;; Canonical? (non-reducable terms)
;; --------------------------------
(define canonical?/Success
  (redex-match? λCon-Success T))

;; Reducible? (non-canonical terms)
;; --------------------------------
(define reducible?/Success
  (redex-match? λCon-Success Reducible))

;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Success~~> ς configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation Success-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Success~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Success-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))