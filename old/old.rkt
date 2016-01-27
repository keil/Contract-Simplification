#lang racket





(bound? (term x) (term (λ x x)))

;  ((L M N) K x (λ x M) (M N) (op M ...))



;; Test λCon-Baseline/ Done
;; ========================

(done? (term (+ 1 2))) ; expect #t
(done? (term ((λ x (+ x 1)) 1))) ; expect #t
(done? (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1))) ; expect #f

(done? (term ((λ x (+ x 1)) (1 @ Nat?)))) ; expect #f
(done? (term ((λ x (+ x (1 @ Nat?))) 1))) ; expect #f
(done? (term ((λ x (+ (x @ Nat?) 1)) 1))) ; expect #f