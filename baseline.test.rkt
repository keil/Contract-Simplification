#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "contracts.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test λCon-Baseline/ Done
(done? (term (+ 1 2)))
(done? (term ((λ x (+ x 1)) 1)))
(done? (term (((λ x (+ x 1)) @ (,Nat? → ,Nat?)) 1)))

(done? (term ((λ x (+ x 1)) (1 @ ,Nat?))))
(done? (term ((λ x (+ x (1 @ ,Nat?))) 1)))
(done? (term ((λ x (+ (x @ ,Nat?) 1)) 1)))

;; Test λCon-Baseline/ Reduction
(test-->> Baseline-reduction (term ((1 @ ,Nat?) @ ,Nat?)) (term 1))
(test-->> Baseline-reduction (term ((0 @ ,Nat?) @ ,Nat?)) (term 0))

;(test-->> Baseline-reduction (term (assert (assert 0 ,Pos?) ,Nat?)) (term blmae))
(test-->> Baseline-reduction (term ((0 @ ,Nat?) @ ,Pos?)) (term blame))

(test-->> Baseline-reduction (term ((λ x (+ x 1)) (1 @ ,Nat?))) (term ((λ x (+ x 1)) 1)))
(test-->> Baseline-reduction (term ((λ x (+ x (1 @ ,Nat?))) 1)) (term ((λ x (+ x 1)) 1)))

(test-->> Baseline-reduction (term ((λ x (+ (x @ ,Nat?) 1)) 1)) (term ((λ x (+ (x @ ,Nat?) 1)) 1)))
 
(test-->> Baseline-reduction (term (1 @ (,Nat? → ,Nat?))) (term 1)) 
(test-->> Baseline-reduction (term (x @ (,Nat? → ,Nat?))) (term (x @ (,Nat? → ,Nat?))))



; Test  λCon-Baseline2/ Reduction

(test-->
 Baseline-reduction2
 (term
  ((λ f (f 1)) ((λ x (+ x 1)) @ (,Num? → ,Num?))))
 (term
  ((λ f ((f @ (,Num? → ,Num?)) 1)) (λ x (+ x 1)))))

(test-->>
 Baseline-reduction2
 (term
  ((λ f (f 1)) ((λ x (+ x 1)) @ (,Num? → ,Num?))))
 (term
  (((λ f (f 1)) (λ x (+ x 1))) @ ,Num?)))







(test-results)