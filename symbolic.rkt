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
  
  ;; TODO
  ;; Predicates
  (P (flat (λ x M)) named)
  
  ;; Values
  ;;((U V W) .... ?)
  ;; Values
  ((U V W) K (λ x M) ?)
  
  ;; Symbolic Values
  ((S T) (V P ...) (S @ Q))
  
  ;; Terms
  ((L M N) .... S)
  
  ;; Contexts
  ((E F) hole (E N) (S E) (op S ... E M ...) (E @ C) (S @ (eval E)))
  )

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
  [(Δ numeric S ...) ((δ/ numeric (unpack S) ...) Num?)]  
  [(Δ logical S ...) ((δ/ logical (unpack S) ...) Bool?)]
  [(Δ relational S ...) ((δ/ relational (unpack S) ...) Bool?)]
  [(Δ predicates S ...) ((δ/ predicates (unpack S) ...) Bool?)]
  )

;; do i really need this unpacking?

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic
   
      (--> (in-hole E V)
           (in-hole E (V))
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
   ;; From λCon
   ;; =========
   ;   (--> (in-hole E (assert v C))
   ;        (in-hole E (v @ C))
   ;        "Assert"
   ;   )
   (--> (in-hole E ((V P ...) @ P_n)) ;; Check if the predicate is valid? // only store values taht are satisfied?
        (in-hole E (V P ... P_n))
        ;(in-hole E (V @ (eval (M V))))
        "Flat"
        )
   
   ;(--> (in-hole E ((V P ...) @ Q)) 
   ;     (in-hole E ((V @ Q) P ... ))
   ;     ;(in-hole E (V @ (eval (M V))))
   ;     "Function"
   ;     )
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


;; Test λCon/ Reduction
(test-->> Symbolic-reduction (term ((+ 1 2) @ Nat?)) (term (3 Num? Nat?)))
(test-->> Symbolic-reduction (term ((+ 1 2) @ Any?)) (term (3 Num? Any?)))
(test-->> Symbolic-reduction (term ((+ 1 2) @ None?)) (term (3 Num? None?))) ;(+blame @ (Pos? None?))

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
