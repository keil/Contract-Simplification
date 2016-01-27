#lang racket


Rational?;; Predefined Predicates
;; =====================

(define-metafunction λCon
  ≼ : P P -> boolean?

  [(≼ Complex? Nummber?) #t]
  
  [(≼ Real? Nummber?) #t]
  [(≼ Rational? Real?) #t]
  [(≼ Integer? Rational?) #t]
  
  
  ;; Default Case
  [(≼ any_0 any_1) #f]
)



(define-metafunction λCon
  lookup/ : predefined -> I
  
  ;; Top/Botyom level
  ;[(lookup/ ⊤) (λ x #t)] ;; TODO, required
  ;[(lookup/ ⊥) (λ x #f)] ;; TODO, required
  
  ;; First Level
  [(lookup/ Nummber?) (⊤ / (λ x (number? x)))]
  [(lookup/ Complex?) (⊤ / (λ x (complex? x)))] ;; but is also subset of Number?
  [(lookup/ Real?) (⊤ / (λ x (real? x)))] ;; but is also subset of Number?
  [(lookup/ Rational?) (⊤ / (λ x (rational? x)))] ;;  ⇒ Real?)
  [(lookup/ Integer?) (⊤ / (λ x (integer? x)))] ;; ⇒ Rational?

  [(lookup/ Exact?) (Number? / (λ x (exact? x)))]
  [(lookup/ Inexact?) (Number? / (λ x (inexact? x)))]
  [(lookup/ Zero?) (Number? / (λ x (zero? x)))]

  [(lookup/ Positive?) (Real? / (λ x (positive? x)))]
  [(lookup/ Negative?) (Real? / (λ x (negative? x)))]
  
  ;; Sixth Level
  [(lookup/ Even?) (Integer? / (λ x (even? x)))]
  [(lookup/ Odd?) (Integer? / (λ x (odd? x)))]
  
  [(lookup/ String?) (⊤ / (λ x (string? x)))]
  [(lookup/ Boolean?) (⊤ / (λ x (boolean? x)))]
  
  ;; Third Level
  [(lookup/ Real?) (⊤ / (λ x (real? x)))] ;  ⇒ Number?
  

  
  ;; Fourth Level
  

  
  
(define-metafunction λCon
  lookup/ : predefined -> (flat P)
  
  ;; First level
  [(lookup/ ⊤) (λ x #t)]
  
  ;; Second Level
  [(lookup/ Nummber?) (⊤ / (λ x (number? x)))]
  [(lookup/ String?) (⊤ / (λ x (string? x)))]
  [(lookup/ Boolean?) (⊤ / (λ x (boolean? x)))]
  
  ;; Third Level
  [(lookup/ Real?) ((λ x (real? x)) ⇒ Number?)]
  [(lookup/ Zero?) ((λ x (zero? x)) ⇒ Number?)]
  [(lookup/ Exact?) ((λ x (exact? x)) ⇒ Number?)]
  [(lookup/ Inexact?) ((λ x (inexact? x)) ⇒ Number?)]
  
  ;; Fourth Level
  [(lookup/ Rational?) ((λ x (rational? x)) ⇒ Real?)]
  [(lookup/ Positive?) ((λ x (positive? x)) ⇒ Real?)]
  [(lookup/ Negative?) ((λ x (negative? x)) ⇒ Real?)]
  
  ;; Fifth Level
  [(lookup/ Integer?) ((λ x (integer? x)) ⇒ Rational?)]
  
  ;; Sixth Level
  [(lookup/ Even?) ((λ x (even? x)) ⇒ Integer?)]
  [(lookup/ Odd?) ((λ x (odd? x)) ⇒ Integer?)]
  
  
  ;; Cross-predicates
  
  [(lookup/ Natatural?) ((λ x (or (> x 0) (= x 0))) ⇒ Num?)]
  
  
  (define-metafunction λCon
  ≤ : P P -> boolean
  [(≤ P ⊤) #t]
  [(≤ P P) #t]
  ;; Chain Lookup
  [(≤ (M ⇒ P_r) P) (≤ P_r P)]
  ;; End
  [(≤ any any) #f])
