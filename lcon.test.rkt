#lang racket
(require redex)

(require "lcon.rkt")
(require "contracts.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test λCon/ Syntax
(redex-match λCon M (term (1 @ (flat 1))))
(redex-match λCon M (term ((+ 1 2) @ (flat 1))))

(redex-match λCon M (term (1 @ (flat (+ 1 1)))))
(redex-match λCon M (term ((+ 1 2) @ (flat (+ 1 1)))))

(redex-match λCon M (term ((λ x (+ x 1)) @ ((flat 1) → (flat 1)))))
(redex-match λCon M (term (((λ x (+ x 1)) @ ((flat 1) → (flat 1))) 1)))

(redex-match λCon M (term ((λ x 1) blame)))

;; Test λCon/ Reduction
(test-->> λCon-reduction (term ((+ 1 2) @ (flat (λ x 1)))) (term 3))
(test-->> λCon-reduction (term ((+ 1 2) @ ,Any?)) (term 3))
(test-->> λCon-reduction (term ((+ 1 2) @ ,None?)) (term blame))

(test-->> λCon-reduction (term (((λ x (+ x 1)) @ (,Nat? → ,Nat?)) 1)) (term 2))

(test-->> λCon-reduction (term (((λ x (+ x 1)) @ (,Pos? → ,Pos?)) 0)) (term blame)) 
(test-->> λCon-reduction (term (((λ x (- x 1)) @ (,Pos? → ,Pos?)) 1)) (term blame))

(test-->> λCon-reduction (term ((((λ x (λ y (+ x y))) @ (,Pos? → (,Pos? → ,Pos?))) 1) 1)) (term 2))

(test-->> λCon-reduction (term ((λ x (x 1)) ((λ x (+ x 1)) @ (,Pos? → ,Pos?)))) (term 2))

(test-->> λCon-reduction (term ((((λ y (λ x ((y x) 1))) @ ((,Pos? → (,Pos? → ,Pos?)) → (,Pos? → ,Pos?))) (λ x (λ y (+ x y)))) 1)) (term 2))

(test-results)