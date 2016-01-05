#lang racket
(require redex)
(require racket/format)

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

(define-language λJ

  ;; Terms
  ((L M N) K x (λ x M) (M N) (op M ...))

  ;; Constants 
  (K number boolean string)
    
  ;; Variables
  ((x y z) variable-not-otherwise-mentioned)
  
  ;; Operations
  (op + * - / < > =)
  
  ;; Values
  ((U V W) K (λ x M))
      
  ;; Contexts
  ((E F) hole (E N) (V E) (op V ... E M ...))
)

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define-metafunction λJ
  subst : x any any -> any
  [(subst x any (λ x M)) (λ x M)]
  [(subst x any (λ y e)) (λ y (subst x any e))]
  [(subst x any x) any]
  [(subst x any y) y]
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2) any_2]
)

(define namespace (make-base-namespace))
(define-metafunction λJ
  δ : op K ... -> K
  [(δ op K ...) ,(eval (term (op K ...)) namespace)]
)

(define λJ-reduction
  (reduction-relation
   λJ
   (--> (in-hole E (op V ...))
        (in-hole E (δ op V ...))
        "δ"
   )
   (--> (in-hole E ((λ x M) V))
        (in-hole E (subst x V M))
        "β"
   )
))