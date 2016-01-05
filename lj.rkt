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
  ;; stop substitution if x is bound
  [(subst x any (λ x M)) (λ x M)]
  ;; unroll substitution
  [(subst x any (λ y e)) (λ y (subst x any e))]
  ;; replatex x
  [(subst x any x) any]
  ;; don't replace x if x and y are different
  [(subst x any y) y]
  ;; all other terms
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2) any_2])

;(define-metafunction λJ
;  δ : (op V ...) -> W
;  [(δ (+ V ...)) ,(+ (term V) ...)]
;  [(δ (* v w)) ,(* (term w) (term v))]
;  [(δ (- v w)) ,(- (term w) (term v))]
;  [(δ (/ v w)) ,(/ (term w) (term v))]
;  [(δ (< v w)) ,(if (< (term v) (term w)) (term 1) (term 0))]
;  [(δ (> v w)) ,(if (> (term v) (term w)) (term 1) (term 0))]
;  [(δ (= v w)) ,(if (= (term v) (term w)) (term 1) (term 0))]
;)

(define-metafunction λJ
  δ : (op K ...) -> K
  [(δ (+ K ...)) (term (+ K ...))]
;    [(δ (+ K ...)) ,(eval (term (op K ...)))]
)

(define λJ-reduction
  (reduction-relation
   λJ
   (--> (in-hole E (op V ...))
        (in-hole E (δ (op V ...)))
        "δ"
   )
   (--> (in-hole E ((λ x M) V))
        (in-hole E (subst x V M))
        "β"
   )
))
(string 1)
(string-append 1 "1")
(redex-match λJ M (term (+ 1 1)))
(traces  λJ-reduction (term (+ 1 1)))