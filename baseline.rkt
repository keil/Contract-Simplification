#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

(require "contracts.rkt")

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
))

;; Unroll union ?


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

;; TODO, shoudl M be obpimized before applying another rule ?
;; Lift: All types of contracts? .. generalize λ y M to M

(define Baseline-reduction2
  (extend-reduction-relation Baseline-reduction
   λCon-Baseline
   
   ;; Unroll
   (--> (in-hole H ((λ x M) ((λ y N) @ Q)))
        (in-hole H ((λ x (unroll x Q M)) (λ y N)))
        "Unroll"
   )
   
   ;; Unfold
   (--> (in-hole H ((R @ (C → D)) T))
        (in-hole H ((R (T @ C)) @ D))
        "Unfold/Function"
   )
   (--> (in-hole H ((S @ (Q ∩ R)) T))
        (in-hole H (((S @ Q) @ R) T))
        "Unfold/Intersection"
   )

   ;; Lower (down)
   (--> (in-hole H (λ x (R @ C)))
        (in-hole H ((λ x R) @ (,Any? → C)))
        "Lower"
   )
   
   ;; Lift (up)
   (--> (in-hole H (λ x (R (x @ C))))
        (in-hole H ((λ x (R x)) @ (C → ,Any?)))
        "Lift"
   )
   
   
   ;; can this rule be more general as swapping may
   ;; also work with v @ C
   
   ;; Swap
   (--> (in-hole H (λ x ((λ y M) (z @ C))))
        (in-hole H ((λ y (λ x M)) (z @ C)))
        "Swap"
   )
   
   ;; Flatten (factorize)
   (--> (in-hole H (op S ... (x @ C) N ...))
        (in-hole H (((λ y (op S ... x N ...)) @ (C → ,Any?)) x))
        "Flatten"
        (fresh y)
   ) 
   ;; TODO, is factorize restricted to valiable names ?
   ;; what if (+ (+ 1 2) @ @, )
   ;; TOfactorizeDO, introduce new variable name
   
   
   
   
   ;; Lift (up)
   ;(--> (in-hole H (λ x ((λ y M) (x @ C)))) ;; All types of contracts? .. generalize λ y M to M
   ;     (in-hole H ((λ x (λ y M)) @ (C → ,Any?)))
   ;     "Lift"
   ;)
   ;; Lift
   ;(--> (in-hole H (λ x (S @ C)))
   ;     (in-hole H ((λ x S) @ (,Any? → C)))
   ;     "Lift"
   ;)
   

   
   ;; TODO eta reduction
   

      
   ;; Propagate
   ;(--> (in-hole H (λ x ((λ y M) (z @ C)))) ;; All types of contracts?
   ;     (in-hole H ((λ y (λ z M)) (z @ C)))
   ;     "Propagate"
   ;)
   ;; push , flatten propagate
   
         ;; Pull
;   (--> (in-hole H (op S ... (M @ C) N ...)) ;; all types of cvontract ?
;        (in-hole H (((λ x (op S ... x N ...)) @ (C → ,Any?)) M))
;        "Flatten"
;   ) ;; TODO, introduce new variable name
   
   
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
  (term ((λ x (+ (x @ ,Num?) 1)) 1)))

;(reduce example-4)
;(traces Baseline-reduction2 example-4)

; TODO, order of reduction steps

(define 
  example-5
  (term (λ x (λ y (+ (x @ ,Num?) y)))))

;(reduce example-5)
;(traces Baseline-reduction2 example-5)

(define 
  example-6
  (term (λ x (λ y (+ (x @ ,Num?) (y @ ,Num?))))))

;(reduce example-6)
;(traces Baseline-reduction2 example-6)





;(variable-not-in (term (+ x 1)) (term x))
;(variable-not-in (term (+ x 1)) (term y))

;(reduce example-2)
;(traces Baseline-reduction2 example-2)


;(traces Baseline-reduction2 example-1)
;(traces Baseline-reduction2 example-addOne1)
;(traces Baseline-reduction2 example-addOne2)
;(traces Baseline-reduction2 