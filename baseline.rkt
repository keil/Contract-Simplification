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
  
  ;; Final terms (top-level contracted)
  (R 
   ;; All contract-free terms
   T
   
   ;; Imemdiate Contracts
   ((op S ...) @ I)
   ((S T) @ I)
   
   ;; Delayed Contracts
   ((λ x T) @ Q)
   (x @ Q)
   ((S T) @ Q)
   
   )
  
  ;; Baseline Reduction Context
  ((G H) hole (λ x H) (H M) (R H) (op R ... H M ...) (H @ C))
  
  
  
  ;; Final Terms
  ;; -----------
  ;; Final terms are all non-reducible terms,
  ;; e.g. Immediate Contracts in variables (x @ (flat M))
;  ((R S T) 
   ;; Term from λJ
;   K x (λ x T) (S T) (op T ...)
   ;; Term from λCon
   ;; - Non-reducible contract assertions (should be liftet)
   ;; - Blame
   ;; - Delayed contracts (at top level, or on X)
   ;;(x @ I))
   
   ;;((λ x T) @ Q)
   
   ;; Immeidate Contract
;   ((op S ...) @ I)
;   ((S T) @ I) ;; will this mean that λ x ((S T) @ C) is final?
   
   ;(x @ I) -- gets liftet
  
   ;; Delayed Contracts
;   ((λ x T) @ Q)
   ;((S T) @ Q)
   ;(x @ Q), wenn nicht in einer applikation
   ; und das nur innerhalb von applikationen oder op's
   
   ;; TODO, union is missing
;   )

  
  
  
  ;; Final Terms
  (final R)
  
  ;; TODO, say the optimization is finished if only 
  ;; one top level delayed contarcts remains
  
  ;; Final Terms
  ;(R S (R_1 R_2) (R @ C) ((λ x R) @ C) (x @ C))
  
  
  ;; - Top-level function contract

  ;; TODO
  ;; One top-level function-contract might remain
  ;; because it cannot be reduced
  ;; (as it is the final interface description)
  ;;((λ x M) @ Q))
  
  ;; TODO, for testing
  ;((C D) .... (C • D))
  

  
  
  
  ;; Execution Body (name?)
  (a K x (a_1 a_2) (op a ...)) ;; Contracts a @ C
  (A hole (op a ... H M ...) (H M) (a H) (H @ C)) ;; Todo (H @ Q)
  
)

;(define 
;  (done? M)
;  (redex-match? λCon-Baseline M))
(define 
  (done? M)
  (redex-match? λCon-Baseline R M))


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

(define Baseline-reduction
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
))

;; TODO
;; Unroll union ?


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

(define Baseline-reduction2
  (extend-reduction-relation Baseline-reduction
   λCon-Baseline
   
   ;; Unroll
   (--> (in-hole H ((λ x M) (V @ Q)))
        (in-hole H ((λ x (unroll x Q M)) V))
        "Unroll"
   )
   
   ;; Unfold
   (--> (in-hole H ((R @ (C → D)) T))
        (in-hole H ((R (T @ C)) @ D))
        "Unfold/Function"
   )
   (--> (in-hole H ((R @ (Q ∩ R)) T))
        (in-hole H (((R @ Q) @ R) T))
        "Unfold/Intersection"
   )

   ;; TODO, shoudl this be a canonical contract
   ;; Qc are either (⊤ → Qc) or (Qc ∩ Qc)?
   ;; or (Qc → Qc)
   ;; also in 
   
   ;; Lower (down)
   (--> (in-hole H (λ x (M @ C)))
        (in-hole H ((λ x M) @ (⊤ → C)))
        "Lower"
   )

   ;; Lift (up) Contract
   ;; ------------------
   ;; Rule [Lift] lifts an immediate contract I
   ;; on argument x and creates a new function contract.
   
   (--> (in-hole H (λ x (in-hole A (x @ I)))) ;; ? all contracts?
        (in-hole H ((λ x (in-hole A (x))) @ (I → ⊤)))
        "Lift"
   )
   
   ;; Lift (up)
   ;(--> (in-hole H (λ x (M (x @ C))))
   ;    (in-hole H ((λ x (M x)) @ (C → ,Any?)))
;
;   "Lift"
;   )
   
   
   ;; Collapse
   ;; --------
   
   ;; TODO, what kind of contracts should be collapsed?
   ;; For example:
   ;; - Flat contarcts: (flat M) • (flat N) --> (flat (M • N))
   ;; - Function contracts: (⊤ → Num) • (Num → ⊤) --> (Num → Num)
   ;; - Function contracts: (⊤ → Num) • (⊤ → Pos) --> (⊤ → (Num • Pos))
   
   ; Collapse
   ;(--> (in-hole H ((V @ Q) @ R))
   ;     (in-hole H (V @ (Q • R)))
   ;     "Collaps"
   ;)
 
))

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



