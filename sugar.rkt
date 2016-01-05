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

(define-extended-language λCon2 λCon
  
  ;; terms (final terms)
  ((r s t) 
   c (λ x r) ;; values
   ((λ x e) @ Q) ;; values 
   x (op r s) (r s) (x @ C) 
           (assert x C) (assert (λ x r) Q)
           )
  
  ;; reduction context
  ((R S T) 
   hole
   (op R e)
   (op r R)
   (R e)
   (r R)
   (assert R C)
   (R @ C)
   )
)

;; predicates
(define done? (redex-match? λCon2 r))

(done? (term (+ 1 2)))
(done? (term ((λ x (+ x 1)) 1)))
(done? (term ((assert (λ x (+ x 1)) (,Nat → ,Nat)) 1)))

(done? (term ((λ x (+ x 1)) (assert 1 ,Nat))))
(done? (term ((λ x (+ x (assert 1 ,Nat))) 1)))
(done? (term ((λ x (+ (assert x ,Nat) 1)) 1)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define Flat-reduction
  (reduction-relation
   λCon2
   (--> (in-hole R (v @ (flat e)))
        (in-hole R (apply-reduction-relation λCon-reduction (v @ (flat e))))
        "Red-Flat"
   )
   (--> (in-hole R (assert v (flat e)))
        (in-hole R (apply-reduction-relation λCon-reduction (v @ (flat e))))
        "Red-2-Flat"
   )
   (--> (in-hole R (assert c (C → D)))
        (in-hole R (c))
        "Red-Function"
   )
))
