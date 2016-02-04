#lang racket

### 
TODO
own test and traces function that previsouly checks the syntax

make a reduction function which immediately un-packs the confoguration
Test for each contarct


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