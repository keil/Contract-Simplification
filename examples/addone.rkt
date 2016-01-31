#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

(define 
  example/addone/0
  (term (((λ f (λ x ((f 1) x)))
          (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))))
         1)
        ))

(redex-match? λCon M example/addone/0)
;(traces λCon-reduction example/addone/0) ;; reduction steps: 10

(define 
  example/addone/1
  (term (((λ f (λ x ((f 1) x)))
          ((λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y)))) @ ((Num? → Num?) ∩ (Str? → Str?))))
         1)
        ))

(redex-match? λCon M example/addone/1)
(traces λCon-reduction example/addone/1) ;; reduction steps: 21

(traces λCon-reduction (reduce example/addone/1)) ;; reduction steps: 189