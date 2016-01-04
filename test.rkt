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
(redex-match λ_J e (term 1))
(redex-match λ_J e (term x))

(redex-match λ_J e (term (+ 1 1)))
(redex-match λ_J e (term (* 1 1)))

(redex-match λ_J e (term ((λ x (+ x 1)) 1)))
(redex-match λ_J e (term (((λ x (λ y (+ x y))) 1) 1)))
(redex-match λ_J e (term ((λ x (x 1)) (λ x x))))

;; Test λ_J/ Reduction
(test-->> λ_J-reduction (term 1) (term 1))
(test-->> λ_J-reduction (term x) (term x))

(test-->> λ_J-reduction (term (+ 1 1)) (term 2))
(test-->> λ_J-reduction (term (* 1 1)) (term 1))

(test-->> λ_J-reduction (term (- 1 1)) (term 0))
(test-->> λ_J-reduction (term (/ 1 1)) (term 1))
(test-->> λ_J-reduction (term (> 1 2)) (term 0))
(test-->> λ_J-reduction (term (< 1 2)) (term 1))
(test-->> λ_J-reduction (term (= 1 2)) (term 0))
(test-->> λ_J-reduction (term (= 1 1)) (term 1))

(test-->> λ_J-reduction (term (λ x 1)) (term (λ x 1)))
(test-->> λ_J-reduction (term ((λ x 1) 1)) (term 1))

(test-->> λ_J-reduction (term ((λ x (+ x 1)) 1)) (term 2))
(test-->> λ_J-reduction (term (((λ x (λ y (+ x y))) 1) 1)) (term 2))
(test-->> λ_J-reduction (term ((λ x (x 1)) (λ x x))) (term 1))

;; Test λ_Con/ Syntax
(redex-match λ_Con e (term (assert 1 (flat 1))))
(redex-match λ_Con e (term (assert (+ 1 2) (flat 1))))

(redex-match λ_Con e (term (assert 1 (flat (+ 1 1)))))
(redex-match λ_Con e (term (assert (+ 1 2) (flat (+ 1 1)))))

(redex-match λ_Con e (term (assert (λ x (+ x 1)) ((flat 1) → (flat 1)))))
(redex-match λ_Con e (term ((assert (λ x (+ x 1)) ((flat 1) → (flat 1))) 1)))

(redex-match λ_Con e (term ((λ x 1) blame)))

;; Test λ_Con/ Reduction
(test-->> λ_Con-reduction (term ((+ 1 2) @ (flat (λ x 1)))) (term 3))
(test-->> λ_Con-reduction (term ((+ 1 2) @ ,Any)) (term 3))
(test-->> λ_Con-reduction (term ((+ 1 2) @ ,Blame)) (term blame))

(test-->> λ_Con-reduction (term ((assert (λ x (+ x 1)) (,Nat → ,Nat)) 1)) (term 2))

(test-->> λ_Con-reduction (term ((assert (λ x (+ x 1)) (,Pos → ,Pos)) 0)) (term blame)) 
(test-->> λ_onC-reduction (term ((assert (λ x (- x 1)) (,Pos → ,Pos)) 1)) (term blame))

(test-->> λ_Con-reduction (term (((assert (λ x (λ y (+ x y))) (,Pos → (,Pos → ,Pos))) 1) 1)) (term 2))

(test-->> λ_Con-reduction (term ((λ f (f 1)) (assert (λ x (+ x 1)) (,Pos → ,Pos)))) (term 2))

(test-->> λ_Con-reduction (term (((assert (λ plus (λ x ((plus x) 1))) ((,Pos → (,Pos → ,Pos)) → (,Pos → ,Pos))) (λ x (λ y (+ x y)))) 1)) (term 2))