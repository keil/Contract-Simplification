#lang racket
(require redex)

(require "../lcon.rkt")
(require "../baseline.rkt")

(provide (all-defined-out))

;; Even-Odd
;; =========
;; Tests recursive contract checking.

;; # 0
;; ---

(define 
  example/evenodd/0
  (term (· 
   ;; even? x
   ((((λ f (λ g (λ x (((f f) g) x))))
      ; even f
      (λ f (λ g (λ x (if (= x 0) #t (((g g) f) (- x 1)))))))
     ; odd g
     (λ g (λ f (λ x (if (= x 0) #f (((f f) g) (- x 1)))))))
    10)
   )))

;; Notes
;; -----
;; Reduction steps: 69

(traces λCon-reduction example/evenodd/0) 



;; # 1
;; ---

(define 
  example/evenodd/1
  (term (· 
   ;; even? x
   ((((λ f (λ g (λ x (((f f) g) x))))
      ; even f
      (λ f (λ g ((λ x (if (= x 0) #t (((g g) f) (- x 1)))) @ ♭0 (Natural? → Boolean?)))))
     ; odd g
     (λ g (λ f ((λ x (if (= x 0) #f (((f f) g) (- x 1)))) @ ♭1 (Natural? → Boolean?)))))
    10)
   )))

;; Notes
;; -----
;; Reduction steps: 201

(traces λCon-reduction example/evenodd/1)