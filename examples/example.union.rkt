#lang racket
(require redex)
(require rackunit)

(require "../lj.rkt")
(require "../lcon.rkt")

(provide (all-defined-out))

;; Blame-example from the ICFP paper
;; =================================

;; # 0
;; ---

(define 
  example/union/0
  (term ((λ f (f 1)) (λ x x))))

;(traces λJ-reduction example/union/0)

;; # 1
;; ---

(define 
  example/union/1
  (term (· ((λ f ((λ z (f 1)) (f 1))) ((λ x x) @ ♭ ((Any? → Number?) ∪ (Any? → String?)))))))

;(traces λCon-reduction example/union/1)

;; # 2
;; ---

(define 
  example/union/2
  (term (· ((λ f ((λ z (f "1")) (f 1))) ((λ x x) @ ♭ ((Any? → Number?) ∪ (Any? → String?)))))))

;(traces λCon-reduction example/union/2)

;; # 3
;; ---

(define 
  example/union/3
  (term (· ((λ f ((λ z (f 1)) (f "1"))) ((λ x x) @ ♭ ((Any? → Number?) ∪ (Any? → String?)))))))

(traces λCon-reduction example/union/3)