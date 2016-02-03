#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

(define 
  example/evenodd/0
  (term 
   ;; even? x
   ((((λ f (λ g (λ x (((f f) g) x))))
      ; even f
      (λ f (λ g (λ x (if (= x 0) #t (((g g) f) (- x 1)))))))
     ; odd g
     (λ g (λ f (λ x (if (= x 0) #f (((f f) g) (- x 1)))))))
    10)
   ))

(redex-match? λCon M example/evenodd/0)
;(traces λCon-reduction example/evenodd/0) ;; reduction steps: 69



(define 
  example/evenodd/1
  (term 
   ;; even? x
   ((((λ f (λ g (λ x (((f f) g) x))))
      ; even f
      (λ f (λ g ((λ x (if (= x 0) #t (((g g) f) (- x 1)))) @ (Nat? → Bool?)))))
     ; odd g
     (λ g (λ f ((λ x (if (= x 0) #f (((f f) g) (- x 1)))) @ (Nat? → Bool?)))))
    10)
   ))

(redex-match? λCon M example/evenodd/1)
;(traces λCon-reduction example/evenodd/1) ;; reduction steps: 124
(traces λCon-reduction (reduce example/evenodd/1)) ;; reduction steps: 189


;(traces λCon-reduction example/evenodd/1) ;; reduction steps: 124