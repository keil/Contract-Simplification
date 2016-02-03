#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")

(provide (all-defined-out))

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Term Containment (≼)
;; ====================

(check-eq?
 (term (≼ (λ x (< x 0)) (λ x (< x 0))))
 #t)

(check-eq?
 (term (≼ (λ x (< x 0)) (λ z (< z 0))))
 #t)

(check-eq?
 (term (≼ (λ x (< x 0)) (λ x (<= x 0)))) 
 #f)

(check-eq?
 (term (≼ (λ x (< x 0)) (λ y (<= y 0))))
 #f)

(check-eq?
 (term (≼ (λ x (number? x)) (λ y (real? y))))
 #f)

(check-eq?
 (term (≼ (λ x (real? x)) (λ y (number? y))))
 #t)

;; Predicate Containment (≤)
;; =========================

(check-eq?
 (term (≤ Number? Number?))
 #t)

(check-eq?
 (term (≤ Number? ⊤))
 #t)

(check-eq?
 (term (≤ ⊤ Number?))
 #f)

(check-eq?
 (term (≤ Real? ⊤))
 #t)

(check-eq?
 (term (≤ Real? Number?))
 #t)


(check-eq?
 (term (≤ Zero? Number?))
 #t)

(check-eq?
 (term (≤ Positive? Real?))
 #t)

(check-eq?
 (term (≤ Natural? Number?))
 #t)

(check-eq?
 (term (≤ Natural? Real?))
 #t)

(check-eq?
 (term (≤ Positive? Natural?))
 #t)

(check-eq?
 (term (≤ Odd? Real?))
 #t)

(check-eq?
 (term (≤ UInteger? Integer?))
 #t)

(check-eq?
 (term (≤ UInteger? Natural?))
 #t)

(check-eq?
 (term (≤ UEven? Even?))
 #t)

(check-eq?
 (term (≤ UOdd? Odd?))
 #t)

(check-eq?
 (term (≤ UOdd? Real?))
 #t)

;(term (Σ Positive?))
;(term (Σ Natural?))









;; Semantic Definition of ⊑
;; ------------------------
;; If C ⊑ D then \forall 
;; * V \in [[C]]+ => V \in [[D]]+
;; * E \in [[C]]- => E \in [[D]]-
;; resp.
;; * V \not\in [[D]]+ => V \not\in [[C]]+
;; * E \not\in [[D]]- => E \not\in [[C]]-
;; 
;; C ⊑ D iff. C is more restrictive than D
;;    E[[ M @ D ]] --> +blame/-blame
;; => E[[ M @ C ]] --> +blame/-blame
;; resp.
;;    E[[ M @ C ]] --> V
;; => E[[ M @ D ]] --> V


;; Flat 
;; ----

(check-eq?
 (term (⊑ (flat (λ x 1)) (flat (λ x 1))))
 #t)

(check-eq?
 (term (⊑ Number? Number?))
 #t)

(check-eq?
 (term (⊑ Natural? Number?))
 #t)

;; Function 
;; --------

(check-eq?
 (term (⊑ (Number? → Number?) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ (Natural? → Number?) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ (Number? → Natural?) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ (Natural? → Natural?) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ (Number? → Number?) (Natural? → Number?)))
 #f)

(check-eq?
 (term (⊑ (Number? → Number?) (Number? → Natural?)))
 #f)

(check-eq?
 (term (⊑ (Number? → Number?) (Natural? → Natural?)))
 #f)

;; Intersection 
;; ------------

(check-eq?
 (term (⊑ (Number? → Number?) ((Number? → Number?) ∩ (Number? → Number?)))) 
 #t)

(check-eq?
 (term (⊑ (Number? → Number?) ((Number? → Number?) ∩ (String? → String?)))) 
 #t)

(check-eq?
 (term (⊑ (Natural? → Natural?) ((Number? → Number?) ∩ (String? → String?)))) 
 #t)

(check-eq?
 (term (⊑ (Natural? → Natural?) ((Natural? → Number?) ∩ (String? → String?)))) 
 #t)

(check-eq?
 (term (⊑ (Natural? → Natural?) ((Number? → Natural?) ∩ (String? → String?)))) 
 #t)

(check-eq?
 (term (⊑ (Number? → Number?) ((Natural? → Natural?) ∩ (String? → String?)))) 
 #f)

(check-eq?
 (term (⊑ (Natural? → Number?) ((Natural? → Natural?) ∩ (String? → String?)))) 
 #f)

(check-eq?
 (term (⊑ (Number? → Natural?) ((Natural? → Natural?) ∩ (String? → String?)))) 
 #f)

(check-eq?
 (term (⊑ (Natural? → Natural?) ((Natural? → Natural?) ∩ (Natural? → String?)))) 
 #f) 

(check-eq?
 (term (⊑ ((Number? → Number?) ∩ (Number? → Number?)) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ ((Natural? → Natural?) ∩ (Number? → Number?)) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ ((Number? → Number?) ∩ (String? → String?)) (Number? → Number?)))
 #f)

(check-eq?
 (term (⊑ ((Natural? → Natural?) ∩ (String? → String?)) (Number? → Number?)))
 #f)

(check-eq?
 (term (⊑ ((Number? → Number?) ∩ (Number? → String?)) (Number? → Number?)))
 #t)

(check-eq?
 (term (⊑ ((Natural? → Natural?) ∩ (Pos? → Pos?)) (Pos? → Pos?))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Natural?) ((⊤ → Natural?) ∩ (⊤ → String?)))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Natural?) ((⊤ → Number?) ∩ (⊤ → String?)))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Number?) ((⊤ → Natural?) ∩ (⊤ → String?)))) 
 #f)

(check-eq?
 (term (⊑ ((⊤ → Number?) ∩ (⊤ → String?)) (⊤ → Number?))) 
 #t)

(check-eq?
 (term (⊑ ((⊤ → Number?) ∩ (⊤ → String?)) (⊤ → Natural?))) 
 #f)

(check-eq?
 (term (⊑ ((⊤ → Natural?) ∩ (⊤ → String?)) (⊤ → Number?))) 
 #t)

(check-eq?
 (term (⊑ (Natural? → ⊤) ((Natural? → ⊤) ∩ (String? → ⊤)))) 
 #t)

(check-eq?
 (term (⊑ (Number? → ⊤) ((Natural? → ⊤) ∩ (String? → ⊤)))) 
 #f)

(check-eq?
 (term (⊑ (Natural? → ⊤) ((Number? → ⊤) ∩ (String? → ⊤)))) 
 #t)

(check-eq?
 (term (⊑ ((Number? → ⊤) ∩ (String? → ⊤)) (Number? → ⊤))) 
 #f)

(check-eq?
 (term (⊑ ((Number? → ⊤) ∩ (String? → ⊤)) (Natural? → ⊤))) 
 #f)

(check-eq?
 (term (⊑ ((Natural? → ⊤) ∩ (String? → ⊤)) (Number? → ⊤))) 
 #f)

(check-eq?
 (term (⊑ (Number? → ⊤) (⊤ → Number?))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Number?) (Number? → ⊤))) 
 #f)