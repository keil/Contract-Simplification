#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(require "examples.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

(test-->>
 Baseline-reduction
 example:remain/0
 (term ((λ x 1) @ (Num? → Num?))))

(test-->>
 Baseline-reduction
 example:remain/1
 (term ((λ x x) @ (Num? → Num?))))

(test-->> 
 Baseline-reduction
 example:remain/2
 (term ((λ x x) @ ((Num? → Num?) → (Num? → Num?)))))

(test-->> 
 Baseline-reduction
 example:remain/3
 (term ((λ f (f 1)) ((λ x 1) @ (Num? → Num?)))))

(test-->> 
 Baseline-reduction
 example:remain/4
 (term ((λ f (f #t)) ((λ x 1) @ (Num? → Num?)))))

(test-->> 
 Baseline-reduction
 example:remain/5
 (term ((λ f (f 1)) ((λ x x) @ (Num? → Num?)))))

(test-->> 
 Baseline-reduction
 example:remain/6
 (term ((λ f (f #t)) ((λ x x) @ (Num? → Num?)))))

(test-->> 
 Baseline-reduction
 example:remain/7
 (term (λ f ((f (λ x x)) 1)  ((λ x x) @ ((Num? → Num?) → (Num? → Num?))))))


(test-results)