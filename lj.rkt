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
           ;;variable-not-otherwise-mentioned)
  
  ;; Operations
  (op + * - / < > = and or not number? string? boolean?)
  
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





(define-metafunction λJ
  free? : x any -> (x ...)
  
  [(free? x x) #t] 
  
  [(free? x (λ x M)) #f]
  [(free? x (λ y M)) (free x M)]
  
  [(free? x (M N)) (or (free? x M) (free? x N))]
  
  [(free? x (op M ...)) (or (free? x M) ... )] 
  
  ;; Continue on the structure of M
  [(free? x (any ...)) (or (bound? x any) ...)]
  ;; Return false if none of the previous rules match
  [(free? x any) #f]
)


(define-metafunction λJ
  bound? : x any -> boolean

  [(bound? x (λ x M)) #t]
  [(bound? x (λ y M)) (bound? x M)]
  
  [(bound? x (M N)) (or (bound? x M) (bound? x N))]
  
  ;; Continue on the structure of M
  [(bound? x (any ...)) (or (bound? x any) ...)]
  ;; Return false if none of the previous rules match
  [(bound? x any) #f]
)


(bound? (term x) (term (λ x x)))

;  ((L M N) K x (λ x M) (M N) (op M ...))




;(define-metafunction/extension free-vars lc-num-lang
;  free-vars-num : e -> (x ...)
;  [(free-vars-num number)
;   ()]
;  [(free-vars-num (+ e_1 e_2))
;   (∪ (free-vars-num e_1)
;      (free-vars-num e_2))])








