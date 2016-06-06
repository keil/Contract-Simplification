#lang racket

(traces
 Join-reduction
 (term (·
        (
         (+ (x @ ι1 Number?) (y @ ι11 Number?))
         ∥
         (+ (x @ ι2 String?) (y @ ι22 String?))
         ))))

(traces
 Join-reduction
 (term (·
        (
         (+ (+blame ♭1) (y @ ι11 Number?))
         ∥
         (+ (x @ ι2 String?) (-blame ♭2))
         ))))

(traces
 Join-reduction
 (term (·
        (
         (+ (x @ ι1 Number?) (y @ ι11 Number?))
         ∥
         (+ (x @ ι1 Number?) (-blame ♭2))
         ))))



(traces Subset-reduction (term (((♭ ◃ ι)·) (((λ x (λ y (x @ ι ⊥))) 1) 1))))