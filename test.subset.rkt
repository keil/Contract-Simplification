#lang racket
(require redex)
(require rackunit)

(require "lcon.rkt")

(provide (all-defined-out))

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Semantic Definition of ⊑
;; ------------------------
;; If C ⊑ D then \forall 
;; * V \in [[C]]+ => V \in [[D]]+
;; * E \in [[C]]- => E \in [[D]]-

(check-eq?
 (term (⊑ (flat (λ x 1)) (flat (λ x 1))))
 #t)
(check-eq?
 (term (⊑ Num? Num?))
 #t)

(check-eq?
 (term (⊑ Nat? Num?))
 #t)

(check-eq?
 (term (⊑ (Num? → Num?) (Num? → Num?)))
 #t)


(check-eq?
 (term (⊑ (Nat? → Num?) (Num? → Num?)))
 #t)
(check-eq?
 (term (⊑ (Num? → Nat?) (Num? → Num?)))
 #t)
(check-eq?
 (term (⊑ (Nat? → Nat?) (Num? → Num?)))
 #t)


(check-eq?
 (term (⊑ (Num? → Num?) (Nat? → Num?)))
 #f)
(check-eq?
 (term (⊑ (Num? → Num?) (Num? → Nat?)))
 #f)
(check-eq?
 (term (⊑ (Num? → Num?) (Nat? → Nat?)))
 #f)


(check-eq?
 (term (⊑ ((Num? → Num?) ∩ (Num? → Num?)) (Num? → Num?)))
 #t)

(check-eq?
 (term (⊑ ((Nat? → Nat?) ∩ (Num? → Num?)) (Num? → Num?)))
 #t)

(check-eq?
 (term (⊑ ((Nat? → Nat?) ∩ (Num? → Num?)) (Num? → Num?))) 
 #t)


(check-eq?
 (term (⊑ ((Nat? → Nat?) ∩ (Pos? → Pos?)) (Num? → Num?))) 
 #t)

(check-eq?
 (term (⊑ ((Num? → Num?) ∩ (Str? → Str?)) (Num? → Num?))) 
 #f)

(check-eq?
 (term (⊑ (Nat? → Nat?) ((Num? → Num?) ∩ (Str? → Str?)))) 
 #t)

(check-eq?
 (term (⊑ (Nat? → Nat?) ((Nat? → Str?) ∩ (Str? → Str?)))) 
 #f)

(check-eq?
 (term (⊑ (Nat? → Nat?) ((Nat? → Nat?) ∩ (Nat? → Str?)))) 
 #f)

(check-eq?
 (term (⊑ ((Num? → Num?) ∩ (Num? → Str?)) (Num? → Num?))) 
 #t)

(check-eq?
 (term (⊑ ((⊤ → Num?) ∩ (⊤ → Str?)) (⊤ → Num?))) 
 #t)

(check-eq?
 (term (⊑ ((⊥ → Num?) ∩ (⊤ → Str?)) (⊤ → Num?))) 
 #f)





