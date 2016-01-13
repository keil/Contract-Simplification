#lang racket


(define-metafunction λJ
  free? : x any -> (x ...)
  
  [(free? x x) #t] 
  
  [(free? x (λ x M)) #f]
  [(free? x (λ y M)) (free x M)]
  
  [(free? x (M N)) (or (free? x M) (free? x N))]
  
  [(free? x (op M ...)) (or (free? x M) ... )] 
  
  ;; Continue on the structure of M
  [(free? x (any ...)) (or (bound? x any) ...)]
  ;; Return false if none of the previous rules match
  [(free? x any) #f]
)


(define-metafunction λJ
  bound? : x any -> boolean

  [(bound? x (λ x M)) #t]
  [(bound? x (λ y M)) (bound? x M)]
  
  [(bound? x (M N)) (or (bound? x M) (bound? x N))]
  
  ;; Continue on the structure of M
  [(bound? x (any ...)) (or (bound? x any) ...)]
  ;; Return false if none of the previous rules match
  [(bound? x any) #f]
)


(bound? (term x) (term (λ x x)))

;  ((L M N) K x (λ x M) (M N) (op M ...))
