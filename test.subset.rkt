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
 (term (⊑ Num? Num?))
 #t)

(check-eq?
 (term (⊑ Nat? Num?))
 #t)

;; Function 
;; --------

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

;; Intersection 
;; ------------

(check-eq?
 (term (⊑ (Num? → Num?) ((Num? → Num?) ∩ (Num? → Num?)))) 
 #t)

(check-eq?
 (term (⊑ (Num? → Num?) ((Num? → Num?) ∩ (Str? → Str?)))) 
 #t)

(check-eq?
 (term (⊑ (Nat? → Nat?) ((Num? → Num?) ∩ (Str? → Str?)))) 
 #t)

(check-eq?
 (term (⊑ (Nat? → Nat?) ((Nat? → Num?) ∩ (Str? → Str?)))) 
 #t)

(check-eq?
 (term (⊑ (Nat? → Nat?) ((Num? → Nat?) ∩ (Str? → Str?)))) 
 #t)

(check-eq?
 (term (⊑ (Num? → Num?) ((Nat? → Nat?) ∩ (Str? → Str?)))) 
 #f)

(check-eq?
 (term (⊑ (Nat? → Num?) ((Nat? → Nat?) ∩ (Str? → Str?)))) 
 #f)

(check-eq?
 (term (⊑ (Num? → Nat?) ((Nat? → Nat?) ∩ (Str? → Str?)))) 
 #f)

;(check-eq?
; (term (⊑ (Nat? → Nat?) ((Nat? → Nat?) ∩ (Nat? → Str?)))) 
; #f) ;; D blames always (this it is more restrictive)




(check-eq?
 (term (⊑ ((Num? → Num?) ∩ (Num? → Num?)) (Num? → Num?)))
 #t)

(check-eq?
 (term (⊑ ((Nat? → Nat?) ∩ (Num? → Num?)) (Num? → Num?)))
 #t)

(check-eq?
 (term (⊑ ((Num? → Num?) ∩ (Str? → Str?)) (Num? → Num?)))
 #f)

(check-eq?
 (term (⊑ ((Nat? → Nat?) ∩ (Str? → Str?)) (Num? → Num?)))
 #f)

;(check-eq?
; (term (⊑ ((Num? → Num?) ∩ (Num? → Str?)) (Num? → Num?)))
; #t) ;; C blames always (this it is more restrictive)

(check-eq?
 (term (⊑ ((Nat? → Nat?) ∩ (Pos? → Pos?)) (Pos? → Pos?))) 
 #f)









(check-eq?
 (term (⊑ (⊤ → Nat?) ((⊤ → Nat?) ∩ (⊤ → Str?)))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Nat?) ((⊤ → Num?) ∩ (⊤ → Str?)))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Num?) ((⊤ → Nat?) ∩ (⊤ → Str?)))) 
 #f)



(check-eq?
 (term (⊑ ((⊤ → Num?) ∩ (⊤ → Str?)) (⊤ → Num?))) 
 #t)

(check-eq?
 (term (⊑ ((⊤ → Num?) ∩ (⊤ → Str?)) (⊤ → Nat?))) 
 #f)

(check-eq?
 (term (⊑ ((⊤ → Nat?) ∩ (⊤ → Str?)) (⊤ → Num?))) 
 #t)




(check-eq?
 (term (⊑ (Nat? → ⊤) ((Nat? → ⊤) ∩ (Str? → ⊤)))) 
 #t)

(check-eq?
 (term (⊑ (Num? → ⊤) ((Nat? → ⊤) ∩ (Str? → ⊤)))) 
 #f)

(check-eq?
 (term (⊑ (Nat? → ⊤) ((Num? → ⊤) ∩ (Str? → ⊤)))) 
 #t)



(check-eq?
 (term (⊑ ((Num? → ⊤) ∩ (Str? → ⊤)) (Num? → ⊤))) 
 #f)

(check-eq?
 (term (⊑ ((Num? → ⊤) ∩ (Str? → ⊤)) (Nat? → ⊤))) 
 #f)

(check-eq?
 (term (⊑ ((Nat? → ⊤) ∩ (Str? → ⊤)) (Num? → ⊤))) 
 #f)




(check-eq?
 (term (⊑ (Num? → ⊤) (⊤ → Num?))) 
 #f)

(check-eq?
 (term (⊑ (⊤ → Num?) (Num? → ⊤))) 
 #f)