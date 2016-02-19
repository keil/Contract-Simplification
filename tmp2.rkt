 #lang racket
(require redex)

(require "baseline.rkt")



(traces
 Baseline-reduction
 (term (· (((λ f (λ x ((f 1) x))) @ ♭0 ((Number? → (Number? → Number?)) → (Number? → Number?))) ((λ x (λ y (+ x y))) @ ♭1 (Number? → (Number? → Number?)))))))