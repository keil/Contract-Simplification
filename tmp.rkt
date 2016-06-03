#lang racket

((((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y))))) @ ι14 (⊤ → ⊥)) @ ι10 (Number? → Number?))
((((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y))))) @ ι14 (⊤ → ⊥)) @ ι10 (Number? → Number?))




(((λ f (λ x ((f 1) x)))
  (λ x
    (λ y
      (if (or (string? x)
              (string? y))
          (string-append x y)
          (+ x y)))))
 @
 ι10
 (Number? → Number?))

(((λ f
    (λ x
      ((-blame ♭) @ ι11 ⊥)))
  (λ x
    (λ y
      (if (or (string? x)
              (string? y))
          (string-append x y)
          (+ x y)))))
 @
 ι10
 (Number? → Number?))