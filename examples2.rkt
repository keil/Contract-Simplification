#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(provide (all-defined-out))




;; Motivating Example: AddOne
;; --------------------------

(define 
  example:addOne0
  (term 
   ((λ f (f 1)) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne0)

(test-->>
 Baseline-reduction
 example:addOne0
 (term (((λ f (f 1)) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; XXX, with contract on addOne
;; with an intersectionb on plus (overload plus)

(define 
  example:addOne1
  (term ((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne1)

(test-->>
 Baseline-reduction
 example:addOne1
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))


(define 
  example:addOne2
  (term ((λ f (λ x ((f x) 1))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne2)

(test-->>
 Baseline-reduction
 example:addOne2
 (term (((λ f (λ x ((f x) 1))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))



;; Motivating Example: Double
;; --------------------------


(define 
  example:double0
  (term ((λ f (λ x ((f x) x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:double0)

(test-->>
 Baseline-reduction
 example:double0
 (term (((λ f (λ x ((f x) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))





;; Motivating Example: Inc
;; -----------------------

(define 
  example:inc0
  (term ((λ f (λ x (f x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:inc0)

(test-->>
 Baseline-reduction
 example:inc0
 (term (((λ f (λ x (f x))) (λ x (λ y (+ x y)))) @ (Num? → (Num? → Num?)))))
  
  
















;; Example: 1
;; ---------

(define 
  example:1
  (term ((λ f (f 1)) ((λ x (+ x 1)) @ (Num? → Num?)))))

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Note: Matthias Keil
; [Lower] moves only function/delayed contracts
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;(traces Baseline-reduction example:1)

;(test-->>
; Baseline-reduction
; example:1
; (term (((λ f (f 1)) (λ x (+ x 1))) @ Num?)))



;; Example: 2
;; ----------

(define 
  example:2
  (term ((λ f ((f 1) @ Nat?)) (λ x (+ x 1)))))

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Note: Matthias Keil
; [Lower] moves only function/delayed contracts
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;(traces Baseline-reduction example:2)

;(test-->>
; Baseline-reduction
; example:2
; (term (((λ f (f 1)) (λ x (+ x 1))) @ Nat?)))







;; Contract at different prosiitions
;; on plus, as contract on the outer function
;; and with or without concrete top-level application






(define 
  example:addOne3
  (term 
   (((λ f (λ x ((f 1) x))) @ ((Num? → (Num? → Num?)) → (Num? → Num?))) (λ x (λ y (+ x y))))))

;(traces Baseline-reduction example:addOne3)

(test-->>
 Baseline-reduction
 example:addOne3
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

  

(define 
  example:addOne4
  (term 
   (((λ f (λ x ((f 1) x))) @ ((Num? → (Num? → Num?)) → (Num? → Num?))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

(traces Baseline-reduction example:addOne4)

(test-->>
 Baseline-reduction
 example:addOne4
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))



;; Test with intersection


;; Predicates

#|

|#

;(traces
; Baseline-reduction
; (term (λ x ((x @ ,Nat?) @ ,Pos?))))



(test-results)