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
  ((S T) K x (λ x T) (S T) (op T ...))
  
  ;; Final terms (only top-level contracted)
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
  (final B)
  
  ;; Baseline Reduction Context
  ((G H) hole (λ x H) (H M) (B H) (op B ... H M ...) (H @ C))
  
  
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
  (A hole (op a ... H M ...) (H M) (a H) (H @ C)) ;; Todo (H @ Q)
  
  ;; Extend contracts
  ((C D) .... (C • D))

)

(define 
  (canonical? C)
  (redex-match? λCon-Baseline Qc C))


;; Done
;; ----
;; Test if an expression is in a final state

(define 
  (done? M)
  (redex-match? λCon-Baseline final M))


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
   
   (--> (in-hole H (V @ I))
        (in-hole H ,(car (apply-reduction-relation* λCon-reduction (term (V @ I)))))
        "Verify"
   )
   
   (--> (in-hole H (K @ Q))
        (in-hole H K)
        "Skip/Constant"
   )
   
   (--> (in-hole H ((op M ...) @ Q))
        (in-hole H (op M ...))
        "Skip/Operation"
   )
      
   (--> (in-hole H (M @ (C ∪ D)))
        (in-hole H ((M @ C) @ D))
        "Reduce/Union"
   )
   
   (--> (in-hole H (M @ (I ∩ C)))
        (in-hole H ((M @ I) @ C))
        "Reduce/Intersection"
   )

   ; Not required because of [Verify]
   ;(--> (in-hole H (R @ ⊤))
   ;     (in-hole H R)
   ;     "Reduce"
   ;)
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
  (extend-reduction-relation Pre-evaluation
   λCon-Baseline
   
   ;; Unroll
   (--> (in-hole H ((λ x M) (B @ Q))) ;; before Q now Qc (V @ C)
        (in-hole H ((λ x (unroll x Q M)) B))
        "Unroll"
   )

   ;; any intersection needs to be unrolled before
   ;; lift is only allowed to lift Cx contracts
   
   ;; ??
   ;; need to guarantee that I do not call this function again aufter lifting
   ;; otherwise use the speil operator from Dimulas
;   (--> (in-hole H ((λ x M) @ (Q → C)))
;        (in-hole H ((λ x (unroll x Q M)) @ (⊤ → C)))
;        "Unroll/Subject/Domain"
;        (side-condition (not (canonical? (term (Q → D)))))
;   )
;   (--> (in-hole H ((λ x M) @ (Dc → D)))
;        (in-hole H (λ x (M @ D)))
;        "Unroll/Subject/Range"
;        (side-condition (not (canonical? (term (Dc → D)))))
;   )
   

   
   ;; what happens if (λ x M) is also contracted?
   ;; can we say that (λ x M) needs to be unrolled before
   ;;: this merans onbe part is alredy inside of m, ans the other part on V
   ;; onlzy V or is also M @ Qx allowed
   
   ;; Unfold
   (--> (in-hole H ((B @ (C → D)) M))
        (in-hole H ((B (M @ C)) @ D))
        "Unfold/Function"
   )
   (--> (in-hole H ((B @ (Q ∩ R)) M))
        (in-hole H (((B @ Q) @ R) M))
        "Unfold/Intersection"
   )

   ;; TODO, shoudl this be a canonical contract
   ;; Qc are either (⊤ → Qc) or (Qc ∩ Qc)?
   ;; or (Qc → Qc)
   ;; also in 
   
   ;; Lower (down)
   (--> (in-hole H (λ x (B @ C))) ;; C before, now Q
        (in-hole H ((λ x B) @ (⊤ → C)))
        "Lower"
   )

   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (in-hole H (λ x (in-hole A (x @ I)))) ;; ? all contracts?
        (in-hole H ((λ x (in-hole A x)) @ (I → ⊤)))
        "Lift"
   )
   

   
   
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
   (--> (in-hole H ((S @ C) @ D))
        (in-hole H (S @ (collapse C D)))
        "Collaps"
   )
   ; Reverse
   (--> (in-hole H ((V @ Q) @ I))
        (in-hole H ((V @ I) @ Q))
        "Reverse"
   )
 
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



