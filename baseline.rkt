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

(define-extended-language λCon-Baseline λCon
  
  ;; Contract-free terms (λJ terms)
  ;; ------------------------------
  ((S T) K x (λ x T) (S T) (op T ...) (if S T_0 T_1))
  
  ;; Final terms (only top-level contracted)
  ;; ---------------------------------------
  (B 
   ;; Contract-free terms
   T
   ;; Imemdiate Contracts
   ((op S ...) @ I)
   ((S T) @ I)
   ;; Delayed Contracts
   ;((λ x T) @ Q)
   ;(x @ Q)
   (S (T @ Q))
   ;((S T) @ Q) ;; NOT n element of B, because it is M @ Q
   
   ;(M (N @ Q)) (M (N @ Qz))
   
   )
  
  ;; Final Terms
  (final B (B @ C)) ; (B @ Q) (B @ I) --> ()
  ;; Empty contracts
  (true T (true → true))
  
  ;; Baseline Reduction Context
  ;; reame to F
  (F hole (λ x F) (F M) (B F) (op B ... F M ...) (F @ C))
  ;(if F M N) (if B F N) (if B F N)) ;; TODO, if
  
  
  ;; restrict to flat contracts instead of I
  ;; pre-evaluated contracts
  ;(Qc (Dc → Rc))
  ;; Domain contracts
  ;(Dc I (Rc → Dc))
  ;; Range contracts
  ;(Rc ⊤ (Dc → Rc))
  
  
  ;; Execution Body (name?)
  ;; Function Body
  (a K x (a_1 a_2) (op a ...)) ;; Contracts a @ C
  ;(H hole (op a ... F M ...) (F M) (a F) (F @ C)) ;; Todo (H @ Q)
  (H hole (op a ... H M ...) (H M) (a H) (H @ C)) ;; Todo (H @ Q)
  ;;;(A hole (op a ... A M ...) (A M) (a A) (A @ C)) ;; Todo (H @ Q)
  
  ;; Extend contracts
  ((C D) .... (C • D))
  
  
  ;; Canonical Contract
  ;(QC (DC → RC))
  ;(DC ⊤ ⊥ I (RC → DC))
  ;(RC ⊤ ⊥ (DC → RC))
  
  )

(define 
  (canonical? C)
  (redex-match? λCon-Baseline QC C))


;; Done
;; ----
;; Test if an expression is in a final state

(define 
  (done? M)
  (redex-match? λCon-Baseline final M))

;(canonical? (term (Num? → Num?)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Baseline Reduction
;; ------------------
;; Verifies all (immediate) contracts 
;; that can be check at compile time

;; TODO
;; implement top-level blame rule

(define Pre-evaluation
  (reduction-relation
   λCon-Baseline 
   
   (--> (in-hole F (V @ I))
        (in-hole F ,(car (apply-reduction-relation* λCon-reduction (term (V @ I)))))
        "Verify"
        )
   
   (--> (in-hole F (K @ Q))
        (in-hole F K)
        "Skip/Constant"
        )
   
   (--> (in-hole F ((op M ...) @ Q))
        (in-hole F (op M ...))
        "Skip/Operation"
        )
   
   (--> (in-hole F (M @ (C ∪ D)))
        (in-hole F ((M @ C) @ D))
        "Reduce/Union"
        )
   
   (--> (in-hole F (M @ (I ∩ C)))
        (in-hole F ((M @ I) @ C))
        "Reduce/Intersection"
        )
   
   (--> (in-hole F (V @ true))
        (in-hole F V)
        "Reduce/True"
        )
   ))

;; Function unroll : x Q M -> N
;; ----------------------------
;; Unrolls a delayed contract Q of function x 
;; to all uses of x

(define-metafunction λCon-Baseline
  unroll : x Q any -> any
  
  ;; Don't continue if x is bound λ's body
  [(unroll x Q (λ x M)) (λ x M)]
  
  ;; Continue unrollong on λ's body
  [(unroll x Q (λ y M)) (λ y (unroll x Q M))]
  
  ;; Put contract to the usage of x
  [(unroll x Q x) (x @ Q)]
  
  ;; Continue unrollong on the structure of M
  [(unroll x Q (any ...)) ((unroll x Q any) ...)]
  
  ;; Return the target expression M if
  ;; none of the previous rules match
  [(unroll x Q any) any]
  )

;; Contract Propagration
;; ----------------------------
;; Applies Contract-assertion rules at compile time whereever possible and
;; lifts contract to the enclosing module boundaries

;; TODO, shoudl M be obpimized before applying another rule ?
;; Lift: All types of contracts? .. generalize λ y M to M

(define Baseline-reduction
  (extend-reduction-relation
   Pre-evaluation
   λCon-Baseline
   
   ;; Unroll
   ;; ------
   ;; Rule [Unroll] unrolles the contract of a contracted argument 
   ;; to all uses of the argument.
   
   (--> (in-hole F ((λ x M) (B @ Q)))
        (in-hole F ((λ x (unroll x Q M)) B))
        "Unroll")
   
   ;; Unfold
   ;; ------
   ;; Rule [Unfold] unfolds a function contract (intersection contract).
   
   (--> (in-hole F ((B @ (C → D)) M))
        (in-hole F ((B (M @ C)) @ D))
        "Unfold/Function")
   
   (--> (in-hole F ((B @ (Q ∩ R)) M))
        (in-hole F (((B @ Q) @ R) M))
        "Unfold/Intersection")
   
   ;; Lower (down)
   ;; ------------
   ;; Rule [Lower] creates a new function cntract from the 
   ;; contract of the function's body
   
   (--> (in-hole F (λ x (B @ C)))
        (in-hole F ((λ x B) @ (⊤ → C)))
        "Lower")
   
   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (in-hole F (λ x (in-hole H (x @ I)))) ;; ? all contracts?
        (in-hole F ((λ x (in-hole H x)) @ (I → ⊤)))
        "Lift")
   
   ;; Collapse
   ;; --------
   
   ;; TODO, what kind of contracts should be collapsed?
   ;; For example:
   ;; - Flat contarcts: (flat M) • (flat N) --> (flat (M • N))
   ;; - Function contracts: (⊤ → Num) • (Num → ⊤) --> (Num → Num)
   ;; - Function contracts: (⊤ → Num) • (⊤ → Pos) --> (⊤ → (Num • Pos))
   
   ; Collapse
   ;(--> (in-hole H ((S @ C) @ D))
   ;     (in-hole H (S @ (collapse C D)))
   ;     "Collaps"
   ;)
   (--> (in-hole F ((S @ Q) @ R))
        (in-hole F (S @ (collapse Q R)))
        "Collaps/Delayed")
   
   (--> (in-hole F ((S @ I) @ J))
        (in-hole F (S @ (collapse I J)))
        "Collaps/Immediate")
   
   ;; Reverse
   ;; -------
   ;; Rule [Reverse] moves delayed contracts to the outer possition.
   ;; This enables to unfold the contract on the outer position.
   ;; Hint: This rules changes the blame-semantcs 
   
   (--> (in-hole F ((M @ Q) @ I))
        (in-hole F ((M @ I) @ Q))
        "Reverse"
        (side-condition (not (λCon-value? (term (M @ Q))))))
   
   ))








;; Function unroll : x Q M -> N
;; ----------------------------
;; Unrolls a delayed contract Q of function x 
;; to all uses of x

;; blame perserving
;; constraint set

(define-metafunction λCon-Baseline
  collapse : C D -> C
  
  ;; +++++++++++++
  ;; Special Rules
  ;; -------------
  ;; Delete this if predicate refinement is implemented
  [(collapse ⊤ C) C]
  [(collapse C ⊤) C]
  ;; +++++++++++++
  
  ;; Collapse flat contarcts.
  [(collapse (flat M) (flat N)) ((flat M) • (flat N))]
  
  ;; Collapse function contract.
  ;; reverse order of preduicates, depending on the evaluation order
  [(collapse (C_l → D_l) (C_r → D_r)) ((collapse C_r C_l) → (collapse D_l D_r))]
  
  ;; +++++++++++++++++++++++++++++++++++
  ;; Default, if not otherwise mentioned
  ;; ((C ∪ D) is unrolled, (I ∩ C) is unrolled)
  ;; TODO, it this correct, or is it also ok
  ;; to remain the sequential assertion @ C @ D
  [(collapse C C) C]
  [(collapse C D) (C • D)]
  )






;; Reduce
;; ------
;; Applies the baseline reduction

(define
  (reduce M)
  (car (apply-reduction-relation* Baseline-reduction M)))





#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#



(define 
  example/addone/0
  (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) 1)))

(redex-match? λCon M example/addone/0)
;(traces λCon-reduction example/addone/0) ;; reduction steps: 6

(define 
  example/addone/1
  (term (((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?)))) 1)))

(redex-match? λCon M example/addone/1)
;(traces λCon-reduction example/addone/1) ;; reduction steps: 14

(reduce example/addone/1)
;(traces Baseline-reduction example/addone/1)
;(traces λCon-reduction (reduce example/addone/1)) ;; reduction steps: 8



(define 
  example-4
  (term ((λ x (+ (x @ Num?) 1)) 1)))

;(reduce example-4)
;(traces Baseline-reduction2 example-4)

; TODO, order of reduction steps

(define 
  example-5
  (term (λ x (λ y (+ (x @ Num?) y)))))

;(reduce example-5)
;(traces Baseline-reduction2 example-5)

(define 
  example-6
  (term (λ x (λ y (+ (x @ Num?) (y @ Num?))))))

;(reduce example-6)
;(traces Baseline-reduction2 example-6)



