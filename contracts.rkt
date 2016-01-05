#lang racket
(require redex)
(require "lcon.rkt")

(provide (all-defined-out))

#|
  ___         _               _      
 / __|___ _ _| |_ _ _ __ _ __| |_ ___
| (__/ _ \ ' \  _| '_/ _` / _|  _(_-<
 \___\___/_||_\__|_| \__,_\__|\__/__/
                                     
|#


(define Any 
  (term (flat (λ x #t))))

(define Blame
  (term (flat (λ x #f))))

(define Pos
  (term (flat (λ x (> x 0)))))

(define Nat
  (term (flat (λ x (& (> x 0) (= x 0))))))

;; examples 
;(traces λ_C-reduction (term ((+ 1 2) @ (flat (λ x 1)))))
;(traces λ_C-reduction (term ((+ 1 2) @ ,Any)))

;(traces λ_C-reduction (term ((assert (λ x (+ x 1)) (,Nat → ,Nat)) 1)))

;(traces λ_C-reduction (term ((assert (λ x (+ x 1)) (,Pos → ,Pos)) 0)))
;(traces λ_C-reduction (term ((assert (λ x (- x 1)) (,Pos → ,Pos)) 1)))

;(traces λ_C-reduction (term (((assert (λ x (λ y (+ x y))) (,Pos → (,Pos → ,Pos))) 1) 1)))

;(traces λ_C-reduction (term ((λ f (f 1)) (assert (λ x (+ x 1)) (,Pos → ,Pos)))))

;(traces λ_C-reduction (term (((assert (λ plus (λ x ((plus x) 1))) ((,Pos → (,Pos → ,Pos)) → (,Pos → ,Pos))) (λ x (λ y (+ x y)))) 1)))
