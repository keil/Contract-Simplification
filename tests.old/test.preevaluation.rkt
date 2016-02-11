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



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Note: Matthias Keil
; Test are skipped until predicate handling is implemented
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

(test-->>
 Baseline-reduction
 example:verify/0
 (term ((λ x (+ 1 x)) 1)))

(test-->>
 Baseline-reduction
 example:skip/0
 (term (+ 1 1)))

(test-->>
 Baseline-reduction
 example:skip/1
 (term (+ 1 1)))

(test-->
 Baseline-reduction
 example:unroll/0
 (term ((λ x ((x @ (Num? → Num?)) 2)) (λ x (+ x 1)))))

(test-results)