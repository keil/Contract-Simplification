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

(define-extended-language λCon-Baseline λCon
  
  ;; Contract-free terms (λJ terms)
  ;; ------------------------------
  ((S T) K x (λ x T) (S T) (op T ...) (if S T_0 T_1))
  
  ;; Non-reducable terms (stucked assertions)
  ;; ----------------------------------------
  ((A B) 
   ;; Contract-free terms
   T
   ;; Imemdiate Contracts
   ((A B) @ b I)
   ((op B ...) @ b I)
   ((if A B_0 B_1) @ b I)
   
   ;; Blame terms
   (blame ♭))
  
  ;; Baseline Reduction Context
  ;; --------------------------
  (F hole (λ x F) (F M) (B F) (op B ... F M ...) (if F M N) (if A F N) (if A B F) (F @ b C)) 
  
  ;; Miscellaneous
  ;; -------------
  
  ;; True-Contracts
  (true-contrct T (true → true)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Baseline Reduction
;; ------------------
;; Verifies all (immediate) contracts 
;; that can be check at compile time

;; TODO
;; implement top-level blame rule

(define Pre-evaluation
  (reduction-relation
   λCon-Baseline
   #:domain (ς M)
   
   (--> (ς
         (in-hole F (V @ ♭ C)))
        (((♭ ◃ ι) ς)
         (in-hole F (V @ ι C)))
        "Assert"
        (fresh ι))
   
   ;; Lookup
   (--> (ς
         (in-hole F (V @ ι predefined)))
        (ς
         (in-hole F (V @ ι (lookup predefined))))
        "Lookup")
   
   (--> (ς
         (in-hole F (V @ b (flat M))))
        ((⇓/State ,(car (apply-reduction-relation* λCon-reduction (term (ς (V @ b (flat M)))))))
         (in-hole F (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (ς (V @ b (flat M)))))))))
        "Verify")
   
   (--> (ς
         (in-hole F (K @ ι Q)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F K))
        "Skip/Constant")
   
   (--> (ς
         (in-hole F ((op M ...) @ ι Q)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F (op M ...)))
        "Skip/Operation")
   
   (--> (ς
         (in-hole F (M @ ι (C ∪ D))))
        (((ι ◃ (ι1 ∪ ι2)) ς)
         (in-hole F ((M @ ι1 C) @ ι2 D)))
        "Reduce/Union"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (M @ ι (I ∩ C))))
        (((ι ◃ (ι1 ∩ ι2)) ς)
         (in-hole F ((M @ ι1 I) @ ι2 C)))
        "Reduce/Intersection"
        (fresh ι1 ι2))
   
   (--> (ς
         (in-hole F (V @ ι true)))
        (((ι ◃ (τ #t)) ς)
         (in-hole F V))
        "Reduce/True")
   ))


