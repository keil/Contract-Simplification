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

(define-extended-language λCon-Symbolic λCon
  
  ;; Constants 
  ;(K .... ?)
  
  ;; Values
  ((U V W) .... (V @ ((flat (λ x M)) ...)) ?)
  
  (P (flat (λ x M)) named)
  
  ;; Operations
  (bool-op < > = and or not number? string? boolean?)
  (num-op + * - /)
  )


#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

#|(define-metafunction λJ
  subst : x any any -> any
  [(subst x any (λ x M)) (λ x M)]
  [(subst x any (λ y M)) (λ y (subst x any M))]
  [(subst x any x) any]
  [(subst x any y) y]
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2) any_2]
)|#



(define-metafunction λCon-Symbolic
  δ/ : op V ... -> V ;; use virtual values?
  [(δ/ op K ...) (δ op K ...)]
  [(δ/ op any ...) ?]
  )

(define-metafunction λCon-Symbolic
  Δ : op V ... -> (V @ (C ...))
  
  [(Δ num-op V ...) ((δ/ num-op V ...) @ (Pos?))]  
  [(Δ bool-op V ...) ((δ/ bool-op V ...) @ (Bool?))]
  )

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic
   (--> (in-hole E (op V ...))
        (in-hole E (Δ op V ...))
        "Δ"
        )
   (--> (in-hole E ((λ x M) V))
        (in-hole E (subst x V M))
        "β"
        )
   
   ;; From λCon
   (--> (in-hole E ((V @ (P ...)) @ P_n))
        (in-hole E (V @ (P ... P_n)))
        "Combine"
        )
   
   ;; From λCon
   ;   (--> (in-hole E (assert v C))
   ;        (in-hole E (v @ C))
   ;        "Assert"
   ;   )
   ;; Immediate Contarcts
   ;(--> (in-hole E (V @ (flat M)))
   ;     (in-hole E (V : (flat M)))
   ;     ;(in-hole E (V @ (eval (M V))))
   ;     "Flat"
   ;     )
   ;(--> (in-hole E (V @ (eval W)))
   ;     (in-hole E V)
   ;     "Unit"
   ;     (side-condition (not (false? (term W))))
   ;     ;(side-condition (not (equal? (term W) #f)))
   ;     )
   ;(--> (in-hole E (V @ (eval W)))
   ;     (in-hole E V)
   ;     ;;blame ;; TODO, Change to V and introduce top-level blame
   ;     "Blame"
   ;     (side-condition (false? (term W)))
   ;     )
   (--> (in-hole E (V @ (C ∪ D)))
        (in-hole E ((V @ C) @ D))
        "Union"
        )
   (--> (in-hole E (V @ (I ∩ C)))
        (in-hole E ((V @ I) @ C))
        "Intersection"
        )
   ;; Delayed Contarcts
   (--> (in-hole E ((V @ (C → D)) W))
        (in-hole E ((V (W @ C)) @ D))
        "D-Function"
        )
   (--> (in-hole E ((V @ (x → C)) W)) ;; TODO
        (in-hole E ((V W) @ C))
        "D-Dependent"
        )
   (--> (in-hole E ((V @ (Q ∩ R)) W))
        (in-hole E (((V @ Q) @ R) W))
        "D-Intersection"
        )
   
   
   
   
   ))



(define
  (analyse M)
  (car (apply-reduction-relation* Symbolic-reduction M)))



;; Test λCon/ Syntax
(redex-match λCon M (term (1 @ (flat 1))))
(redex-match λCon M (term ((+ 1 2) @ (flat 1))))

(redex-match λCon M (term (1 @ (flat (+ 1 1)))))
(redex-match λCon M (term ((+ 1 2) @ (flat (+ 1 1)))))

(redex-match λCon M (term ((λ x (+ x 1)) @ ((flat 1) → (flat 1)))))
(redex-match λCon M (term (((λ x (+ x 1)) @ ((flat 1) → (flat 1))) 1)))

(redex-match λCon M (term ((λ x 1) +blame)))
(redex-match λCon M (term ((λ x 1) -blame)))

;; Test λCon/ Reduction
(test-->> Symbolic-reduction (term ((+ 1 2) @ (flat (λ x 1)))) (term 3))

(traces Symbolic-reduction (term ((+ 1 2) @ (flat (λ x 1)))))

;(test-->> Symbolic-reduction (term ((+ 1 2) @ Any?)) (term 3))
;(test-->> Symbolic-reduction (term ((+ 1 2) @ None?)) (term +blame))

;(test-->> Symbolic-reduction (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1)) (term 2))

;(test-->> Symbolic-reduction (term (((λ x (+ x 1)) @ (Pos? → Pos?)) 0)) (term -blame)) 
;(test-->> Symbolic-reduction (term (((λ x (- x 1)) @ (Pos? → Pos?)) 1)) (term +blame))

;(test-->> Symbolic-reduction (term ((((λ x (λ y (+ x y))) @ (Pos? → (Pos? → Pos?))) 1) 1)) (term 2))

;(test-->> Symbolic-reduction (term ((λ x (x 1)) ((λ x (+ x 1)) @ (Pos? → Pos?)))) (term 2))

;(test-->> Symbolic-reduction (term ((((λ y (λ x ((y x) 1))) @ ((Pos? → (Pos? → Pos?)) → (Pos? → Pos?))) (λ x (λ y (+ x y)))) 1)) (term 2))

(test-results)
