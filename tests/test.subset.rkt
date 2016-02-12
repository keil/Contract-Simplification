#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Term Subset (≤)
;; ===============

(check-eq?
 (term (≤ (λ x (< x 0)) (λ x (< x 0))))
 #t)

(check-eq?
 (term (≤ (λ x (< x 0)) (λ z (< z 0))))
 #t)

(check-eq?
 (term (≤ (λ x (< x 0)) (λ x (<= x 0)))) 
 #f)

(check-eq?
 (term (≤ (λ x (< x 0)) (λ y (<= y 0))))
 #f)

(check-eq?
 (term (≤ (λ x (number? x)) (λ y (real? y))))
 #f)

(check-eq?
 (term (≤ (λ x (real? x)) (λ y (number? y))))
 #t)

;; Contract Containment (⊑)
;; ========================

;; Flat Contracts
;; --------------

(check-eq?
 (term (⊑ Number? Number?))
 #t)

(check-eq?
 (term (⊑ Number? ⊤))
 #t)

(check-eq?
 (term (⊑ ⊤ Number?))
 #f)

(check-eq?
 (term (⊑ Real? ⊤))
 #t)

(check-eq?
 (term (⊑ Real? Number?))
 #t)


(check-eq?
 (term (⊑ Zero? Number?))
 #t)

(check-eq?
 (term (⊑ Positive? Real?))
 #t)

(check-eq?
 (term (⊑ Natural? Number?))
 #t)

(check-eq?
 (term (⊑ Natural? Real?))
 #t)

(check-eq?
 (term (⊑ Positive? Natural?))
 #t)

(check-eq?
 (term (⊑ Odd? Real?))
 #t)
