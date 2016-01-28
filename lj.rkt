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
  ((L M N) K x (λ x M) (M N) (op M ...) (if L M N))
  
  ;; Constants 
  (K number boolean string)
  
  ;; Variables
  ((x y z) (variable-prefix x) (variable-prefix y) (variable-prefix z)
           (variable-prefix f) (variable-prefix g) (variable-prefix h))
  
  ;; Primitive Operations
  (op predicates logical numeric relational)
  
  ;; TypeOf Predicates
  (predicates number? complex? real? rational? integer? string? boolean?
              exact? inexact? zero? positive? negative? even? odd?)
  ;; Logical Operators
  (logical and or not)
  ;; Numeric Operators
  (numeric + * - /)
  ;; Relational Operators
  (relational < > = <= >=)
  
  ;; Values
  ((U V W) K (λ x M))
  ;; False Values
  (false #f 0 "")
  
  ;; Evaluation Contexts
  (E hole (E N) (V E) (op V ... E M ...) (if E M N)))

(define λJ-value?
  (redex-match? λJ V))

(define false? 
  (redex-match? λJ false))

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
  [(subst x any_1 any_2) any_2])

(define namespace (make-base-namespace))
(define-metafunction λJ
  δ : op K ... -> K
  [(δ op K ...) ,(eval (term (op K ...)) namespace)])

(define λJ-reduction
  (reduction-relation
   λJ
   (--> (in-hole E (op V ...))
        (in-hole E (δ op V ...))
        "δ")
   
   (--> (in-hole E ((λ x M) V))
        (in-hole E (subst x V M))
        "β")
   
   (--> (in-hole E (if V M N))
        (in-hole E M)
        "if/true"
        (side-condition (not (false? (term V)))))
   
   (--> (in-hole E (if V M N))
        (in-hole E N)
        "if/false"
        (side-condition (false? (term V))))
   ))

(define-metafunction λJ
  free? : x any -> (x ...)
  ;; Check for free valriables
  [(free? x x) #t] 
  [(free? x (λ x M)) #f]
  [(free? x (λ y M)) (free x M)]
  ;; Continue on the structure of M
  [(free? x (any ...)) (or (free? x any) ...)]
  ;; Return false if none of the previous rules match
  [(free? x any) #f])

(define-metafunction λJ
  bound? : x any -> boolean
  ;; Check for bound variables
  [(bound? x (λ x M)) #t]
  [(bound? x (λ y M)) (bound? x M)]
  ;; Continue on the structure of M
  [(bound? x (any ...)) (or (bound? x any) ...)]
  ;; Return false if none of the previous rules match
  [(bound? x any) #f])