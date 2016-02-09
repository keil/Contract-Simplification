#lang racket
  
  ;; Predicate (refinement)
  ;; ----------------------
  (P ⊤ (P / M) predefined)
  
  (predefined %Number %Complex %Real %Rational %Integer %String %Boolean
              %Exact %Inexact %Zero  
              %Positive %Negative %Even %Odd %Natural  
              %UInteger %UEven %UOdd)
  
  #|
 ___            _ _         _         ___          _           _   _          
| _ \_ _ ___ __| (_)__ __ _| |_ ___  | __|_ ____ _| |_  _ __ _| |_(_)___ _ _  
|  _/ '_/ -_) _` | / _/ _` |  _/ -_) | _|\ V / _` | | || / _` |  _| / _ \ ' \ 
|_| |_| \___\__,_|_\__\__,_|\__\___| |___|\_/\__,_|_|\_,_\__,_|\__|_\___/_||_|
                                                                              
|#

;; Sum (Σ)
;; -------
;; Summarizes the patricular predicates of a
;; refinement chain (P).
(define-metafunction λCon
  Σ : P -> (M ...)
  [(Σ ⊤) ()]
  [(Σ predefined) (Σ (lookup predefined))]
  [(Σ (P / M)) (⊎ (Σ P) (M))])

(define-metafunction λCon
  Σ/ : P ... -> (M ...)
  [(Σ/ ) ()]
  [(Σ/ P_0 P_1 ...) (⊎ (Σ P_0) (Σ/ P_1 ...))])

;; Predicate Evaluation (eval)
;; ---------------------------
;; Evaluates a set oof predicates and
;; returns the conjunction of the results.
(define-metafunction λCon
  eval : (M ...) V -> V
  [(eval () V) #t]
  [(eval (M) V) (⇓/Term ,(with-handlers ([(λ x #t) (lambda (exn) (term (· #f)))]) (λCon-->* (term (M V)))))]
  [(eval (M_0 M_1 ...) V) ,(and (term (eval (M_0) V)) (term (eval (M_1 ...) V)))]
  [(eval any ...) #f])
