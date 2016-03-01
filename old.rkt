#lang racket



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
   ;(--> (ς
   ;      (in-hole F ((S @ Q) @ R)))
   ;     (ς
   ;      (in-hole F (S @ (collapse Q R))))
   ;     "Collaps/Delayed")
   
   ;(--> (ς
   ;      (in-hole F ((S @ I) @ J)))
   ;     (ς
   ;      (in-hole F (S @ (collapse I J))))
   ;     "Collaps/Immediate")
   
   ;; Swap
   ;; -------
   ;; Rule [Swap] moves delayed contracts to the outer possition.
   ;; This enables to unfold the contract on the outer position.
   ;; Hint: This rules changes the blame-semantcs 
   
   ;(--> (ς
   ;      (in-hole F ((M @ ι_0 Q) @ ι_1 I)))
   ;     (ς
   ;      (in-hole F ((M @ ι_1 I) @ ι_0 Q)))
   ;     "Swap"
   ;     (side-condition (not (λCon-value? (term (M @ ι_0 Q))))))





  [(is-blame-state? ((♭ ◃ κ) ς)) (or (μ ((♭ ◃ κ) ς) ♭) (is-blame-state? ς))]
  [(is-blame-state? ((ι ◃ κ) ς)) (is-blame-state? ς)]
  [(is-blame-state? ·) #f])



(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))





;(define-metafunction λCon
;  check-blame-state : ς (♭ ...) -> any
;  ;[(check-blame-state ς ()) ]
;  [(check-blame-state ς (♭_0 ♭_1 ...)) ,(if (term (is-false? (μ ς ♭_0)))
;                                            (term ((μ ς ♭_0) ♭_0))
;                                            (term (check-blame-state ς (♭_1 ...)))
;                                            )])


;; Blame of
;; --------
;; Returns the blame for a solution
;(define-metafunction λCon
;  blameOf : ω -> blame
;  [(blameOf ) -blame]
;  [(blameOf (B ∘ #f)) +blame])







;(define-metafunction λCon
;  is-blame-state-for? : ς (♭ ...) -> boolean
;  [(is-blame-state-for? ς ()) #f]
;  [(is-blame-state-for? ς (♭_0 ♭_1 ...)) ,(or 
;                                           (term (is-false? (μ ς ♭_0)))
;                                           (term (is-blame-state-for? ς (♭_1 ...))))])

;(define-metafunction λCon
;  is-blame-state-of? : ς (♭ ...) -> (any any)
;  
;  [(in-blame-state? ς (♭_0 b_1 ...)) ,
;                                     (if (term (is-false? (μ ς ♭_0)))
;                                         (term ((μ ς ♭_0) ♭_0))
;                                         (term (in-blame-state? ς (b_1 ...)))
;                                         )])

;(define-metafunction λCon
;  in-blame-state? : ς -> boolean
;  [(in-blame-state? ς) (check-labels ς (labels ς))])

;(define-metafunction λCon
;  in-blame-state? : ς (♭ ...) -> boolean
;  [(is-blame-state-for? ς ()) #f]
;  [(is-blame-state-for? ς (♭_0 ♭_1 ...)) ,(or 
;                                           (term (is-false? (μ ς ♭_0)))
;                                           (term (is-blame-state-for? ς (♭_1 ...))))])


















;(redex-match? λCon ♭ (term ♭1))
;(variable-not-in (term (+ ♭1 ♭1)) (term ♭))
;(variable-not-in (term (+ ι y)) (term ι))
;(fresh (term (+ ι y)))

(define λCon-value?
  (redex-match? λCon V))




(define-metafunction λCon
  Σ : P -> (M ...)
  [(Σ ⊤) ((λ x #t))]
  [(Σ ⊥) ((λ x #f))]
  ;[(Σ (⊤ / M)) (M)]
  [(Σ (P / M)) (⊕ (Σ P) (M))]
  [(Σ predefined) (Σ (lookup/ predefined))]
  )


(define-metafunction λCon
  ⊕ : (M ...) (M ...) -> (M ...)
  [(⊕ (M ...) ()) (M ...)]
  [(⊕ () (M ...)) (M ...)]
  [(⊕ (M_0 ... M_n M_i ...) (M_n M_m ...)) (⊕ (M_0 ... M_n M_i ...) (M_m ...))]
  [(⊕ (M_0 ...) (M_n M_m ...)) (⊕ (M_0 ... M_n) (M_m ...))])




      (in-hole E (V @ (eval ,(with-handlers 
                                    ([(λ x #t) (lambda (exn) (term #f))])
                                  (evaluate (term (M V))))))))

(side-condition (not (false? (term W))))