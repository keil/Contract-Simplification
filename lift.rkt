#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

(require "symbolic.rkt")
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

(define-extended-language λCon-Lift λCon-Subset)

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

(define Lift-reduction
  (extend-reduction-relation
   Subset-reduction
   λCon-Lift
   #:domain (ς any)
    
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (ς
         (in-hole F (λ x (in-hole BCtx (x @ ι I)))))
        (((ι ◃ (¬ ι1)) ς)
         (in-hole F ((λ x (in-hole BCtx x)) @ ι1 (I → ⊤))))
        "Lift"
        (fresh ι1)
        (side-condition (canonical? (term (in-hole F (λ x (in-hole BCtx (x @ ι I))))))))
   
   (--> (ς
         (in-hole F (λ x (in-hole BCtx (T @ ι ⊥)))))
        (ς
         (in-hole F (λ x (blame ♭))))
        "Blame"
        (where (blame ♭) (blame-of ι ς)))
   
   ))

   ;; Blame
   ;; ---------------
   ;; Removes (term ⊥) contracts.
   
   ;; TODO: ⊥ mus remain
   
;   (--> (ς
;         (in-hole F (λ x (in-hole BCtx (T @ ι ⊥)))))
;        (ς
;         (in-hole F (λ x (blame ♭))))
;        "Blame"
;        (where (blame ♭) (blame-of ι ς)))



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



;; λCon Reduction (λCon-->)
;; ------------------------
(define
  (λCon/Lift~~> ς configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation Lift-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; λCon Reduction (λCon-->*)
;; -------------------------
(define
  (λCon/Lift~~>* configuration)
  (if (redex-match? λCon (ς M) configuration)
      (car (apply-reduction-relation* Lift-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))