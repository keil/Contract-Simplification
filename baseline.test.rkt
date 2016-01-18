#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

;(require "examples.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#


; Test  λCon-Baseline2/ Reduction

(test-->
 Baseline-reduction2
 (term
  ((λ f (f 1)) ((λ x (+ x 1)) @ (Num? → Num?))))
 (term
  ((λ f ((f @ (Num? → Num?)) 1)) (λ x (+ x 1)))))

(traces Baseline-reduction2 (term
  ((λ f (f 1)) ((λ x (+ x 1)) @ (Num? → Num?)))))


(test-->>
 Baseline-reduction2
 (term
  ((λ f (f 1)) ((λ x (+ x 1)) @ (Num? → Num?))))
 (term
  (((λ f (f 1)) (λ x (+ x 1))) @ Num?)))







