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
  ;; -----
  ((L M N) K x (λ x ... M) (M N ...) (op M ...) (if L M N))
  
  ;; Constants 
  ;; ---------
  (K number boolean string)
  
  ;; Variables
  ;; ---------
  ((x y z) (variable-prefix x) (variable-prefix y) (variable-prefix z)
           (variable-prefix f) (variable-prefix g) (variable-prefix h))
  
  ;; Primitive Operations
  ;; --------------------
  (op predicates logicals numerics relationals strings)
  
  (predicates number? complex? real? rational? integer? string? boolean?
              exact? inexact? zero? positive? negative? even? odd?)
  
  (logicals and or not)
  
  (numerics + * - /)
  
  (relationals < > = <= >=)
  
  (strings string-append)
  
  ;; Values
  ;; ------
  ((U V W) K (λ x ... M))
  
  ;; False Values
  (false #f 0 "")
  
  ;; Evaluation Contexts
  ;; -------------------
  (E hole (V ... E M ...) (op V ... E M ...) (if E M N)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define λJ-reduction
  (reduction-relation
   λJ
   (--> (in-hole E (op V ...))
        (in-hole E (δ op V ...))
        "δ")
   
   (--> (in-hole E ((λ x ... M) V ...))
        (in-hole E (subst-n (x V) ... M))
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

#|
 __  __     _            ___             _   _             
|  \/  |___| |_ __ _ ___| __|  _ _ _  __| |_(_)___ _ _  ___
| |\/| / -_)  _/ _` |___| _| || | ' \/ _|  _| / _ \ ' \(_-<
|_|  |_\___|\__\__,_|   |_| \_,_|_||_\__|\__|_\___/_||_/__/

|#

;; Substitution (subst)
;; --------------------
(define-metafunction λJ
  subst-n : (x any) ... any -> any
  [(subst-n (x_0 any_0) (x_i any_i) ... any) (subst-n (x_i any_i) ... (subst x_0 any_0 any))]
  [(subst-n any) any])

(define-metafunction λJ
  subst : x any any -> any
  [(subst y any_1 (λ x ... y z ... any_2)) (λ x ... y z ... any_2)]
  [(subst y any_1 (λ x ... any_2)) (λ x ... (subst y any_1 any_2))]
  [(subst x any x) any]
  [(subst x any y) y]
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2) any_2])

;; Delta (δ)
;; ---------
(define namespace (make-base-namespace))
(define-metafunction λJ
  δ : op V ... -> V
  [(δ op V ...) ,(eval (term (op V ...)) namespace)])

;; Free Variables (free?)
;; ----------------------
(define-metafunction λJ
  free? : x any -> boolean
  [(free? x x) #t] 
  [(free? y (λ x ... y z ... M)) #f]
  [(free? y (λ x ... M)) (free y M)]
  [(free? x (any ...)) (or (free? x any) ...)]
  [(free? x any) #f])

;; Bound Variables (bound?)
;; ------------------------
(define-metafunction λJ
  bound? : x any -> boolean
  [(bound? y (λ x ... y z ... M)) #t]
  [(bound? y (λ x ... M)) (bound? y M)]
  [(bound? x (any ...)) (or (bound? x any) ...)]
  [(bound? x any) #f])

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

;; λJ-Value (λJ-value?)
;; --------------------
(define λJ-value?
  (redex-match? λJ V))

;; λJ-Term (λJ-term?)
;; ------------------
(define λJ-term?
  (redex-match? λJ M))

;; False-Values (false?)
;; ---------------------
(define false? 
  (redex-match? λJ false))

;; λJ Reduction (λJ-->)
;; --------------------
(define
  (λJ--> M)
  (if (redex-match? λJ M M)
      (car (apply-reduction-relation λJ-reduction M))
      (error "Invalid λJ-term:" M)))

;; λJ Reduction (λJ-->*)
;; ---------------------
(define
  (λJ-->* M)
  (if (redex-match? λJ M M)
      (car (apply-reduction-relation* λJ-reduction M))
      (error "Invalid λJ-term:" M)))