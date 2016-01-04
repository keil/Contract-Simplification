#lang racket
(require redex)
(require "lj.rkt")

;; test
(redex-match λ_J e (term 1))
(redex-match λ_J e (term x))

(redex-match λ_J e (term (+ 1 1)))
(redex-match λ_J e (term (* 1 1)))

(redex-match λ_J e (term ((λ x (+ x 1)) 1)))
(redex-match λ_J e (term (((λ x (λ y (+ x y))) 1) 1)))
(redex-match λ_J e (term ((λ x (x 1)) (λ x x))))

;; test
(traces λ_J-reduction (term 1))
(traces λ_J-reduction (term x))

(traces λ_J-reduction (term (+ 1 1)))
(traces λ_J-reduction (term (* 1 1)))

(traces λ_J-reduction (term (- 1 1)))
(traces λ_J-reduction (term (/ 1 1)))
(traces λ_J-reduction (term (> 1 2)))
(traces λ_J-reduction (term (< 1 2)))
(traces λ_J-reduction (term (= 1 2)))
(traces λ_J-reduction (term (= 1 1)))

(traces λ_J-reduction (term (λ x 1)))
(traces λ_J-reduction (term ((λ x 1) 1)))

(traces λ_J-reduction (term ((λ x (+ x 1)) 1)))
(traces λ_J-reduction (term (((λ x (λ y (+ x y))) 1) 1)))
(traces λ_J-reduction (term ((λ x (x 1)) (λ x x))))