#lang racket
  
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

     ;; Lift (up)
   ;(--> (in-hole H (λ x (M (x @ C))))
   ;    (in-hole H ((λ x (M x)) @ (C → ,Any?)))
;
;   "Lift"
;   )
   
  
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
  ;;((C D) .... (C • D))
  

  