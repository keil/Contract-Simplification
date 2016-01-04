#lang racket
(require redex)

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

(define-language λ_J
    
  ;; Comstants 
  (c natural)
    
  ;; Variables
  ((x y z) variable-not-otherwise-mentioned this proto)
  
  ;; Operations
  (op + * - / < > =)
  
  ;; Values
  ((u v w) c (λ x e))
  
  ;; Expressions
  ((e f g)
   v
   x
   (op e f)
   (e f)
  )
      
  ;; Evaluation Contexts
  ((E F G)
   hole
   (op E f)
   (op v F)
   (E f)
   (v F)
  )
)

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define-metafunction λ_J
  subst : x any any -> any
  [(subst x any (λ x e)) (λ x e)]
  [(subst x any (λ y e))  (λ y (subst x any e))]
  [(subst x any x) any]
  [(subst x any y) y]
  ;; all other expressions
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2) any_2])

(define-metafunction λ_J
  δ : e -> u
  [(δ (+ v w)) ,(+ (term v) (term w))]
  [(δ (* v w)) ,(* (term w) (term v))]
  [(δ (- v w)) ,(- (term w) (term v))]
  [(δ (/ v w)) ,(/ (term w) (term v))]
  [(δ (< v w)) ,(if (< (term v) (term w)) (term 1) (term 0))]
  [(δ (> v w)) ,(if (> (term v) (term w)) (term 1) (term 0))]
  [(δ (= v w)) ,(if (= (term v) (term w)) (term 1) (term 0))]
)

(define λ_J-reduction
  (reduction-relation
   λ_J
   (--> (in-hole E (op v w))
        (in-hole E (δ (op v w)))
        "δ"
   )
   (--> (in-hole E ((λ x e) v))
        (in-hole E (subst x v e))
        "App"
   )
))