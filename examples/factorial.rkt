#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))


(define 
  example/factorial/0
  (term 
   (((λ f (λ x ((f f) x))) (λ f ((λ x (if (= x 1) 1 (* x ((f f) (- x 1))))) @ (Nat? → Pos?)))) 5)
 ))

(redex-match? λCon M example/factorial/0)
(traces λCon-reduction example/factorial/0)



(define
  solution/fac
  (term (((λ f
    (λ x
      (((f
         ((f @ ⊤)
          @
          (⊤
           →
           (Pos? → Pos?))))
        @
        (Pos? → Pos?))
       x)))
  (λ f
    (λ x
      (if (= x 1)
        1
        (*
         x
         ((f f) (- x 1)))))))
 5)
   ))