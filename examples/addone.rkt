#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

;; AddOne
;; =========
;; Motivating Example.

;; # 0
;; ---

(define 
  example/addone/0
  (term (· (((λ f (λ x ((f 1) x)))
             (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))
            1))))

;; Notes
;; -----
;; Reduction steps: 10

(traces λCon-reduction example/addone/0)

;; # 1
;; ---

(define 
  example/addone/1
  (term (· (((λ f (λ x ((f 1) x)))
             ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ♭ ((Number? → (Number? → Number?)) ∩ (String? → (String? → String?)))))
            1))))

;; Notes
;; -----
;; Reduction steps: 46

(traces λCon-reduction example/addone/1)