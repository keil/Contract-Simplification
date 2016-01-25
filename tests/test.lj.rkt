#lang racket
(require redex)
(require "lj.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test λ_J/ Syntax
(redex-match λJ M (term 1))
(redex-match λJ M (term x))

(redex-match λJ M (term (+ 1 1)))
(redex-match λJ M (term (* 1 1)))

(redex-match λJ M (term ((λ x (+ x 1)) 1)))
(redex-match λJ M (term (((λ x (λ y (+ x y))) 1) 1)))
(redex-match λJ M (term ((λ x (x 1)) (λ x x))))

;; Test λ_J/ Reduction
(test-->> λJ-reduction (term 1) (term 1))
(test-->> λJ-reduction (term x) (term x))

(test-->> λJ-reduction (term (+ 1 1)) (term 2))
(test-->> λJ-reduction (term (* 1 1)) (term 1))

(test-->> λJ-reduction (term (- 1 1)) (term 0))
(test-->> λJ-reduction (term (/ 1 1)) (term 1))
(test-->> λJ-reduction (term (> 1 2)) (term #f))
(test-->> λJ-reduction (term (< 1 2)) (term #t))
(test-->> λJ-reduction (term (= 1 2)) (term #f))
(test-->> λJ-reduction (term (= 1 1)) (term #t))

(test-->> λJ-reduction (term (λ x 1)) (term (λ x 1)))
(test-->> λJ-reduction (term ((λ x 1) 1)) (term 1))

(test-->> λJ-reduction (term ((λ x (+ x 1)) 1)) (term 2))
(test-->> λJ-reduction (term (((λ x (λ y (+ x y))) 1) 1)) (term 2))
(test-->> λJ-reduction (term ((λ x (x 1)) (λ x x))) (term 1))

(test-results)