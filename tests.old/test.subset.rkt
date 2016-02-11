#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")

(provide (all-defined-out))










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