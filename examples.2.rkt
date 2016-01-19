#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(provide (all-defined-out))


;; Motivating Example: AddOne
;; ==========================

;; AddOne 0
;; --------

(define 
  example:addOne/0
  (term 
   ((λ f (f 1)) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne/0)

(test-->>
 Baseline-reduction
 example:addOne/0
 (term (((λ f (f 1)) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; AddOne 1
;; --------

(define 
  example:addOne/1
  (term ((λ f (λ x ((f 1) x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne/1)

(test-->>
 Baseline-reduction
 example:addOne/1
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; AddOne 2
;; --------

(define 
  example:addOne/2
  (term ((λ f (λ x ((f x) 1))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne/2)

(test-->>
 Baseline-reduction
 example:addOne/2
 (term (((λ f (λ x ((f x) 1))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; AddOne 3
;; --------

(define 
  example:addOne/3
  (term 
   (((λ f (λ x ((f 1) x))) @ ((Num? → (Num? → Num?)) → (Num? → Num?))) (λ x (λ y (+ x y))))))

;(traces Baseline-reduction example:addOne/3)

(test-->>
 Baseline-reduction
 example:addOne/3
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; AddOne 4
;; --------

(define 
  example:addOne/4
  (term 
   (((λ f (λ x ((f 1) x))) @ ((Num? → (Num? → Num?)) → (Num? → Num?))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:addOne/4)

(test-->>
 Baseline-reduction
 example:addOne/4
 (term (((λ f (λ x ((f 1) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; AddOne 5
;; --------

(define 
  example:addOne/5
  (term 
   ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ((Num? → (Num? → Num?)) ∩ (Num? → (Num? → Num?)))))))

;(traces Baseline-reduction example:addOne/5)

(test-->>
 Baseline-reduction
 example:addOne/5
 (term (((λ f (f 1)) (λ x (λ y (+ x y)))) @ (Num? → Num?))))

;; AddOne 6
;; --------

(define 
  example:addOne/6
  (term 
   ((λ f (f 1)) ((λ x (λ y (+ x y))) @ ((Num? → (Num? → Num?)) ∩ (Str? → (Str? → Str?)))))))

;(traces Baseline-reduction example:addOne/6)

(test-->>
 Baseline-reduction
 example:addOne/6
 (term ((λ f ((f (1 @ (Str? • Num?))) @ ((Str? • Num?) → (Num? • Str?)))) (λ x (λ y (+ x y))))))



;; Motivating Example: Double
;; ==========================

(define 
  example:double/0
  (term ((λ f (λ x ((f x) x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:double/0)

(test-->>
 Baseline-reduction
 example:double/0
 (term (((λ f (λ x ((f x) x))) (λ x (λ y (+ x y)))) @ (Num? → Num?))))





;; Motivating Example: Inc
;; =======================

(define 
  example:inc/0
  (term ((λ f (λ x (f x))) ((λ x (λ y (+ x y))) @ (Num? → (Num? → Num?))))))

;(traces Baseline-reduction example:inc/0)

(test-->>
 Baseline-reduction
 example:inc/0
 (term (((λ f (λ x (f x))) (λ x (λ y (+ x y)))) @ (Num? → (Num? → Num?)))))
 

;; Print summary
(test-results)