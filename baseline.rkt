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

  ;; Final Terms
  ;; -----------
  ;; Final terms are all non-reducible terms,
  ;; e.g. Immediate Contracts in variables (x @ (flat M))
  ((R S T) 
   ;; Term from λJ
   K x (λ x T) (S T) (op T ...)
   ;; Term from λCon
   ;; - Non-reducible contract assertions (should be liftet)
   ;; - Blame
   ;; - Delayed contracts (at top level, or on X)
   ;;(x @ I))
   
   ;; Immeidate Contract
   ((op S ...) @ I)
   ((S T) @ I)
;   (x @ I) -- gets liftet
  
   ;; Delayed Contracts
   ;((S T) @ Q)
   ;((λ x S) @ Q)
   
   ;; TODO, union is missing
   )

  ;; Final Terms
  (final T )
  
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
  
  ;; Baseline Reduction Context
  ((G H) hole (λ x H) (op S ... H M ...) (H M) (S H) (H @ C)) ;; Todo (H @ Q)
  
  
  
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
   (--> (in-hole H ((M @ (C → D)) N))
        (in-hole H ((M (N @ C)) @ D))
        "Unfold/Function"
   )
   (--> (in-hole H ((M @ (Q ∩ R)) N))
        (in-hole H (((M @ Q) @ R) N))
        "Unfold/Intersection"
   )

   
   
   
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
   (--> (in-hole H ((V @ C) @ D))
        (in-hole H (V @ (C • D)))
        "Collaps"
   )
   
   ;; swap
   
   ;; flatten
   
   
   
   

   
   

   
  
   
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



