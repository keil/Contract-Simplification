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
  
  ((U V W) .... ?)
  
  ;; Virtual Values/ Symbols
  ((S T) V (V @ (P ...)))
  
  ((L M N) .... S)
  
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

(define-metafunction λCon-Symbolic
  unpack : S -> V
  [(unpack (V @  (P ...))) V]
  [(unpack V) V]
  [(unpack any) ?]
 )

(define-metafunction λCon-Symbolic
  δ/ : op S ... -> S
  [(δ/ op K ...) (δ op K ...)]
  [(δ/ op any ...) ?]
  )

(define-metafunction λCon-Symbolic
  Δ : op S ... -> (S @ (C ...))
  [(Δ num-op S ...) ((δ/ num-op (unpack S) ...) @ (Num?))]  
  [(Δ bool-op S ...) ((δ/ bool-op (unpack S) ...) @ (Bool?))]
  )

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic
   
   (--> (in-hole E (op S ...))
        (in-hole E (Δ op S ...))
        "Δ"
        )
   (--> (in-hole E ((λ x M) S))
        (in-hole E (subst x S M))
        "β"
        )
   
   ;; From λCon
   (--> (in-hole E ((S @ (P ...)) @ P_n))
        (in-hole E (S @ (P ... P_n)))
        "Combine"
        )
   
   ;; From λCon
   ;   (--> (in-hole E (assert v C))
   ;        (in-hole E (v @ C))
   ;        "Assert"
   ;   )
   
    (--> (in-hole E (V @ P))
        (in-hole E (V @ (P)))
        ;(in-hole E (V @ (eval (M V))))
        "Flat"
        )
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
   (--> (in-hole E ((V @ (C → D)) S))
        (in-hole E ((V (S @ C)) @ D))
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
   
   ;; aplication of a symnbolic function ?
   ;; say, every value is a sum,bolic value
   
   
   ))

; only alloweed to collect predicates that evaluate to true ?
; reasong is that we need to consider the blame, od checked predictae
; collecting did not mean that the value satisfied the contract/ only that it is checked against this contract
; but without a check of th blame state this is useless


(define
  (analyse M)
  (car (apply-reduction-relation* Symbolic-reduction M)))


;; Test λCon/ Reduction
(test-->> Symbolic-reduction (term ((+ 1 2) @ Nat?)) (term (3 @ (Num? Nat?))))

(test-->> Symbolic-reduction (term ((+ 1 2) @ Any?)) (term (3 @ (Num? Any?))))
(test-->> Symbolic-reduction (term ((+ 1 2) @ None?)) (term (3 @ (Num? None?)))) ;(+blame @ (Pos? None?))

(test-->> Symbolic-reduction (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1)) (term (2 @ (Num? Nat?))))

(test-->> Symbolic-reduction (term (((λ x (+ x 1)) @ (Pos? → Pos?)) 0)) (term (1 @ (Num? Pos?))))
(test-->> Symbolic-reduction (term (((λ x (- x 1)) @ (Pos? → Pos?)) 1)) (term (0 @ (Num? Pos?))))

(test-->> Symbolic-reduction (term ((((λ x (λ y (+ x y))) @ (Pos? → (Pos? → Pos?))) 1) 1)) (term (2 @ (Num? Pos?))))

(test-->> Symbolic-reduction (term ((λ x (x 1)) ((λ x (+ x 1)) @ (Pos? → Pos?)))) (term (2 @ (Num? Pos?))))

(test-->> Symbolic-reduction (term ((((λ y (λ x ((y x) 1))) @ ((Pos? → (Pos? → Pos?)) → (Pos? → Pos?))) (λ x (λ y (+ x y)))) 1)) (term (2 @ (Num? Pos? Pos?))))

(test-results)
