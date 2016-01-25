#lang racket

   
      
   ;; Flatten (factorize)
;   (--> (in-hole H (op M ... (N @ C) L ...))
;        ;(in-hole H ((λ x (op S ... x L ...)) (N @ C)))
;        (in-hole H (((λ x (op M ... x L ...)) @ (C → ,Any?)) N))
;        "FlattenX" ;; introduce new boundaries/ 
;        (fresh x)
;   )
   
   
   
   
   
   ;; Collapse
   ;(--> (in-hole H ((S @ C) @ D)) ;; Only delayed contarcts? Only on values
   ;     (in-hole H (S @ (C • D))) ;; Did not work for more than two contract, right?
   ;     "Collaps"
   ;)
   
   
   


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

;; TODO eta reduction ?

;(variable-not-in (term (+ x 1)) (term x))
;(variable-not-in (term (+ x 1)) (term y))

;(reduce example-2)
;(traces Baseline-reduction2 example-2)


 ;; can this rule be more general as swapping may
   ;; also work with v @ C
   
   ;; Swap
   ;(--> (in-hole H (λ x ((λ y M) (z @ C))))
   ;     (in-hole H ((λ y (λ x M)) (z @ C)))
   ;     "Swap"
   ;)
   (--> (in-hole H (((λ x M) (x @ C)) N))
        (in-hole H (((λ x (M N)) (x @ C))))
        "Move?"
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
   

;(traces Baseline-reduction2 example-1)
;(traces Baseline-reduction2 example-addOne1)
;(traces Baseline-reduction2 example-addOne2)
;(traces Baseline-reduction2 