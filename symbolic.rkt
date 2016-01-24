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
  
  ;; Predicates
  (P (flat (λ x M)) named)
  
  ;; Values
  ((U V W) K (λ x M) ?)
  
  ;; Symbolic Values
  ((S T) (V P ...) (S @ Q))
  
  ;; Terms
  ((L M N) .... S)
  
  ;; Contexts
  ((E F) hole (E N) (S E) (op S ... E M ...) (E @ C) (S @ (eval E)))
  
  ;; False Values
  (false .... ?))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define-metafunction λCon-Symbolic
  unpack : S -> V
  [(unpack (V P ...)) V]
  [(unpack V) V]
  [(unpack any) ?]
  )

(define-metafunction λCon-Symbolic
  δ/ : op V ... -> V
  [(δ/ op K ...) (δ op K ...)]
  [(δ/ op any ...) ?]
  )

(define-metafunction λCon-Symbolic
  Δ : op S ... -> S
  [(Δ numeric (V P ...) ...) ((δ/ numeric V ...) Num?)]  
  [(Δ logical (V P ...) ...) ((δ/ logical V ...) Bool?)]
  [(Δ relational (V P ...) ...) ((δ/ relational V ...) Bool?)]
  [(Δ predicates (V P ...) ...) ((δ/ predicates V ...) Bool?)]
  )

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic
   
   (--> (in-hole E V)
        (in-hole E (V ⊤))
        "Abstract"
        )
   
   ;; Rules from λJ
   ;; =============
   (--> (in-hole E (op S ...))
        (in-hole E (Δ op S ...))
        "Δ"
        )
   (--> (in-hole E (((λ x M) P ...) S))
        (in-hole E (subst x S M))
        "β"
        )
   ;; Immediate Contarcts
   (--> (in-hole E ((V P ...) @ (flat M)))
        (in-hole E ((V P ... (flat M)) @ (eval (M V))))
        "Flat"
        )
   (--> (in-hole E ((V P ...) @ named))
        (in-hole E ((V P ...) @ (lookup named)))
        "Lookup"
        )   
   
   (--> (in-hole E ((V P ...) @ (eval (W P_n ...))))
        (in-hole E (V P ...))
        "Unit"
        (side-condition (not (false? (term W))))
        )
   (--> (in-hole E ((V P ... P_n) @ (eval (W P_m ...))))
        (in-hole E (V P ...))
        "Blame"
        (side-condition (false? (term W)))
        )
   
   
   (--> (in-hole E (S @ (C ∪ D)))
        (in-hole E ((S @ C) @ D))
        "Union"
        )
   (--> (in-hole E (S @ (I ∩ C)))
        (in-hole E ((S @ I) @ C))
        "Intersection"
        )
   ;; Delayed Contarcts
   (--> (in-hole E ((S @ (C → D)) T))
        (in-hole E ((S (T @ C)) @ D))
        "D-Function"
        )
   (--> (in-hole E ((S @ (x → C)) T)) ;; TODO
        (in-hole E ((S T) @ C))
        "D-Dependent"
        )
   (--> (in-hole E ((S @ (Q ∩ R)) T))
        (in-hole E (((S @ Q) @ R) T))
        "D-Intersection"
        )   
   ))

; only alloweed to collect predicates that evaluate to true ?
; reasong is that we need to consider the blame, od checked predictae
; collecting did not mean that the value satisfied the contract/ only that it is checked against this contract
; but without a check of th blame state this is useless

;; Symbolic Execution
;; ================== 

(define
  (analyse M)
  (car (apply-reduction-relation* Symbolic-reduction M)))

(traces Symbolic-reduction (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1)))
;; Test λCon/ Reduction
(test-->> Symbolic-reduction (term ((+ 1 2) @ Nat?)) (term (3 Num? Nat?)))
(test-->> Symbolic-reduction (term ((+ 1 2) @ ⊤)) (term (3 Num? ⊤)))
(test-->> Symbolic-reduction (term ((+ 1 2) @ ⊥)) (term (3 Num? ⊥))) ;(+blame @ (Pos? None?))

(test-->> Symbolic-reduction (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1)) (term (2 Num? Nat?)))

(test-->> Symbolic-reduction (term (((λ x (+ x 1)) @ (Pos? → Pos?)) 0)) (term (1 Num? Pos?)))
(test-->> Symbolic-reduction (term (((λ x (- x 1)) @ (Pos? → Pos?)) 1)) (term (0 Num? Pos?)))

(test-->> Symbolic-reduction (term ((((λ x (λ y (+ x y))) @ (Pos? → (Pos? → Pos?))) 1) 1)) (term (2 Num? Pos?)))
;(traces Symbolic-reduction (term ((((λ x (λ y (+ x y))) @ (Pos? → (Pos? → Pos?))) 1) 1)))

(test-->> Symbolic-reduction (term ((λ x (x 1)) ((λ x (+ x 1)) @ (Pos? → Pos?)))) (term (2 Num? Pos?)))
(test-->> Symbolic-reduction (term ((((λ y (λ x ((y x) 1))) @ ((Pos? → (Pos? → Pos?)) → (Pos? → Pos?))) (λ x (λ y (+ x y)))) 1)) (term (2 Num? Pos? Pos?)))
(test-->> Symbolic-reduction (term ((λ x (x 1)) ((λ x (+ x 1)) @ ⊤))) (term (2 Num?)))
;(traces Symbolic-reduction (term ((λ x (x 1)) ((λ x (+ x 1)) @ ⊤))))

(test-results)
