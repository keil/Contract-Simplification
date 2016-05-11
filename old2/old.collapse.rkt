
;; Delayed Contracts
;((λ x T) @ Q)
;(x @ Q)
;(S (T @ Q))
;((S T) @ Q) ;; NOT n element of B, because it is M @ Q

;(M (N @ Q)) (M (N @ Qz))



;; Final Terms
;(final B (B @ C)) ; (B @ Q) (B @ I) --> ()


;; Final Terms
;(final B (B @ C)) ; (B @ Q) (B @ I) --> ()



;; restrict to flat contracts instead of I
;; pre-evaluated contracts
;(Qc (Dc → Rc))
;; Domain contracts
;(Dc I (Rc → Dc))
;; Range contracts
;(Rc ⊤ (Dc → Rc))

(define `
  (canonical? C)
  (redex-match? λCon-Baseline QC C))

;; Extend contracts
((C D) .... (C • D))


;; Canonical Contract
;(QC (DC → RC))
;(DC ⊤ ⊥ I (RC → DC))
;(RC ⊤ ⊥ (DC → RC))






;(canonical? (term (Num? → Num?)))








;; Done
;; ----
;; Test if an expression is in a final state

(define 
  (done? M)
  (redex-match? λCon-Baseline final M))













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




