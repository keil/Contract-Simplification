#lang racket
(require redex)
(require rackunit)

(require "../symbolic.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

(test-->> 
 Symbolic-reduction 
 (term ((+ 1 2) @ Nat?)) 
 (term (3 / Num? Nat?)))

(test-->> 
 Symbolic-reduction 
 (term ((+ 1 2) @ ⊤))
 (term (3 / Num? ⊤)))

(test-->> 
 Symbolic-reduction 
 (term ((+ 1 2) @ ⊥))
 (term (3 / Num?))) ;TODO (+blame @ (Pos? None?))

(test-->> 
 Symbolic-reduction 
 (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1))
 (term (2 / Num? Nat?)))

(test-->> 
 Symbolic-reduction 
 (term (((λ x (+ x 1)) @ (Pos? → Pos?)) 0))
 (term (1 / Num? Pos?)))

(test-->> 
 Symbolic-reduction 
 (term (((λ x (- x 1)) @ (Pos? → Pos?)) 1))
 (term (0 / Num?)))

(test-->> 
 Symbolic-reduction 
 (term ((((λ x (λ y (+ x y))) @ (Pos? → (Pos? → Pos?))) 1) 1))
 (term (2 / Num? Pos?)))

(test-->> 
 Symbolic-reduction 
 (term ((λ x (x 1)) ((λ x (+ x 1)) @ (Pos? → Pos?))))
 (term (2 / Num? Pos?)))

(test-->> 
 Symbolic-reduction 
 (term ((((λ y (λ x ((y x) 1))) @ ((Pos? → (Pos? → Pos?)) → (Pos? → Pos?))) (λ x (λ y (+ x y)))) 1))
 (term (2 / Num? Pos? Pos?)))

(test-->> 
 Symbolic-reduction 
 (term ((λ x (x 1)) ((λ x (+ x 1)) @ ⊤)))
 (term (2 / Num?)))

(test-results)