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

(define-language λJ

  ;; Terms
  ((L M N) K x (λ x M) (M N) (op M ...))

  ;; Constants 
  (K number boolean string)
  
  ;; Variables
  ((x y z) (variable-prefix x) (variable-prefix y) (variable-prefix z)
           (variable-prefix f) (variable-prefix g) (variable-prefix h))
  
  ;; Operations
  (op predicates logical numeric relational)
  
  ;; TypeOf Predicates
  (predicates number? string? boolean?)
  ;; Logical Operators
  (logical and or not)
  ;; Numeric Operators
  (numeric + * - /)
  ;; Relational Operators
  (relational < > =)
  
  ;; Values
  ((U V W) K (λ x M))
      
  ;; Contexts
  ((E F) hole (E N) (V E) (op V ... E M ...))
)

(define λJ-value?
  (redex-match? λJ V))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define-metafunction λJ
  subst : x any any -> any
  [(subst x any (λ x M)) (λ x M)]
  [(subst x any (λ y M)) (λ y (subst x any M))]
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