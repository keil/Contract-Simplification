#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(require "contracts.rkt")

(provide (all-defined-out))

#|
 ___               _ _            ___        _         _   _          
| _ ) __ _ ___ ___| (_)_ _  ___  | _ \___ __| |_  _ __| |_(_)___ _ _  
| _ \/ _` (_-</ -_) | | ' \/ -_) |   / -_) _` | || / _|  _| / _ \ ' \ 
|___/\__,_/__/\___|_|_|_||_\___| |_|_\___\__,_|\_,_\__|\__|_\___/_||_|

|#


;; Example: 0
;; ---------

(define 
  example-0
  (term ((λ x (+ x (1 @ ,Nat?))) 1)))

;(traces
; Baseline-reduction2
; example-0)

(test-->>
 Baseline-reduction2
 example-0
 (term ((λ x (+ x 1)) 1)))
 


;; Example: 1
;; ---------

(define 
  example-1
  (term ((λ f (f 1)) ((λ x (+ x 1)) @ (,Num? → ,Num?)))))

;(traces
; Baseline-reduction2
; example-1)

(test-->>
 Baseline-reduction2
 example-1
 (term (((λ f (f 1)) (λ x (+ x 1))) @ ,Num?)))



;; Example: 2
;; ---------

(define 
  example-2
  (term ((λ f ((f 1) @ ,Nat?)) (λ x (+ x 1)))))

;(traces
; Baseline-reduction2
; example-2)

(test-->>
 Baseline-reduction2
 example-2
 (term (((λ f (f 1)) (λ x (+ x 1))) @ ,Nat?)))




;; Example: addOne 1
;; -----------------

(define 
  example-addOne1
  (term 
   ((λ plus (λ x ((plus 1) x))) ((λ x (λ y (+ x y))) @ (,Num? → (,Num? → ,Num?))))))






  
  
;; Contract at different prosiitions
;; on plus, as contract on the outer function
;; and with or without concrete top-level application

(define 
  example-addOne1
  (term 
   ((λ plus (λ x ((plus 1) x))) ((λ x (λ y (+ x y))) @ (,Num? → (,Num? → ,Num?))))))

(define 
  example-addOne2
  (term 
   (((λ plus (λ x ((plus 1) x))) @ ((,Num? → (,Num? → ,Num?)) → (,Num? → ,Num?))) (λ x (λ y (+ x y))))))












;; Predicates

#|

|#

;(traces
; Baseline-reduction2
; (term (λ x ((x @ ,Nat?) @ ,Pos?))))



(test-results)