#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

(require "contracts.rkt")
(require "examples.rkt")

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
  ((S T) 
   ;; Term from λJ
   K x (λ x T) (S T) (op T ...)
   ;; Term from λCon
   ;; - Non-reducible contract assertions (should be liftet)
   ;; - Blame
   ;; - Delayed contracts (at top level, or on X)
   (x @ I))
  
  ;; - Top-level function contract

  ;; TODO
  ;; One top-level function-contract might remain
  ;; because it cannot be reduced
  ;; (as it is the final interface description)
  ;;((λ x M) @ Q))
  
  ;; Baseline Reduction Context
  ((G H) hole (λ x H) (op S ... H M ...) (H M) (S H) (H @ C)) ;; Todo (H @ Q)
)

(define 
  (done? M)
  (redex-match? λCon-Baseline M))

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
        "Skip"
   )
))

;; Function unroll : x Q M -> N
;; ----------------------------
;; Unrolls a delayed contract Q of function x 
;; to all uses of x

(define-metafunction λCon-Baseline
  unroll : x Q M -> N
  
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

(define Baseline-reduction2
  (extend-reduction-relation Baseline-reduction
   λCon-Baseline
   
   ;; Unroll
   (--> (in-hole H ((λ x M) ((λ y N) @ Q)))
        (in-hole H ((λ x (unroll x Q M)) (λ y N)))
        "Unroll"
   )
   
   ;; Unfold
   (--> (in-hole H ((M @ (C → D)) N)) ;; N should be T (optimized)
        (in-hole H ((M (N @ C)) @ D))
        "Unfold-Function"
   )
   (--> (in-hole H ((S @ (Q ∩ R)) N))
        (in-hole H (((S @ Q) @ R) N))
        "Unfold-Intersection"
   )

   ;; Lift
   ;(--> (in-hole H (λ x (S @ C)))
   ;     (in-hole H ((λ x S) @ (,Any? → C)))
   ;     "Lift"
   ;)
   
   ;; Sink
   (--> (in-hole H (λ x (S @ C)))
        (in-hole H ((λ x S) @ (,Any? → C)))
        "Sink"
   )
   
   
   ;; TODO
   ;; collaps argument contarcts, because teh arg may be used several times
   ;; Move/ Propagate
   ;; Lift Domain
   
   ;; Collapse
   ;(--> (in-hole H ((S @ C) @ D)) ;; Only delayed contarcts?
   ;     (in-hole H (S @ (C • D))) ;; Did not work for more than two contract, right?
   ;     "Collaps"
   ;)
   
   ;; TODO, implement predicate refinement
   ;; and merge contracts
   
   
   
))



;(traces Baseline-reduction2 example-1)
;(traces Baseline-reduction2 example-addOne1)
;(traces Baseline-reduction2 example-addOne2)
;(traces Baseline-reduction2 