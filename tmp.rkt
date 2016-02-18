#lang racket
(require redex)
(require rackunit)




   
   ;(--> (ς
   ;      (in-hole F (K @ ι Q)))
   ;     (((ι ◃ (τ #t)) ς)
   ;      (in-hole F K))
   ;     "Reduce/Constant")
   
   ;(--> (ς
   ;      (in-hole F ((op M ...) @ ι Q)))
   ;     (((ι ◃ (τ #t)) ς)
   ;      (in-hole F (op M ...)))
   ;     "Redcude/Operation")











;; Old verify, update of state not correct beucase it will end in a blame state
;; and thus no execution would be poissible

(--> (ς
      (in-hole F (V @ ι (flat M))))
     ((⇓/State ,(car (apply-reduction-relation* λCon-reduction (term (ς (V @ ι (flat M)))))))
      (in-hole F (⇓/Term ,(car (apply-reduction-relation* λCon-reduction (term (ς (V @ ι (flat M)))))))))
     "Verify")






(require "lcon.rkt")



;; Execution Body (name?)
;; Function Body
(a K x (a_1 a_2) (op a ...)) ;; Contracts a @ C
;(H hole (op a ... F M ...) (F M) (a F) (F @ C)) ;; Todo (H @ Q)
(H hole (op a ... H M ...) (H M) (a H) (H @ C)) ;; Todo (H @ Q)
;;;(A hole (op a ... A M ...) (A M) (a A) (A @ C)) ;; Todo (H @ Q)

(HH )


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

(define 
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
  example:inc/1
  (term ((λ f (λ x (λ y ((f x) y)))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

(traces Baseline-reduction example:inc/1)

(test-->>
 Baseline-reduction
 example:inc/1
 (term ((λ f (λ x (λ y ((f (x @ Num?)) y)))) ((λ x (λ y (+ x y))) @ (⊤ → (Num? → Num?))))))



(define 
  example:inc/2
  (term ((λ f (λ x (λ y ((f y) x)))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:inc/2)

(test-->>
 Baseline-reduction
 example:inc/2
 (term ((λ f (λ x (λ y ((f y) (x @ Num?))))) ((λ x (λ y (+ x y))) @ (⊤ → (Num? → Num?))))))









(define 
  example/addone/0
  (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) 1)))

(redex-match? λCon M example/addone/0)
;(traces λCon-reduction example/addone/0) ;; reduction steps: 6

(define 
  example/addone/1
  (term (((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?)))) 1)))

(redex-match? λCon M example/addone/1)
;(traces λCon-reduction example/addone/1) ;; reduction steps: 14

;(reduce example/addone/1)
;(traces Baseline-reduction example/addone/1)
;(traces λCon-reduction (reduce example/addone/1)) ;; reduction steps: 8



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




(test-results)