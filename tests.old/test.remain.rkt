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

;; Test Remining Contract
;; ======================

(test-->>
 Baseline-reduction
 (term (· ((λ x 1) @ (Number? → Number?))))
 (term (· ((λ x 1) @ (Number? → Number?)))))

(test-->>
 Baseline-reduction
 (term (· ((λ x x) @ (Number? → Number?))))
 (term (· ((λ x x) @ (Number? → Number?)))))

(test-->> 
 Baseline-reduction
 (term (· ((λ x x) @ ((Number? → Number?) → (Number? → Number?)))))
 (term (· ((λ x x) @ ((Number? → Number?) → (Number? → Number?))))))

(test-->> 
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x 1) @ (Number? → Number?)))))
 (term (· (((λ f (f 1)) (λ x 1)) @ ⊤))))

(test-->> 
 Baseline-reduction
 (term (· ((λ f (f #t)) ((λ x 1) @ (Number? → Number?)))))
 (term (· (((λ f (f #t)) (λ x 1)) @ ⊤))))

(test-->> 
 Baseline-reduction
 (term (· ((λ f (f 1)) ((λ x x) @ (Number? → Number?)))))
 (term (· (((λ f (f 1)) (λ x x)) @ Number?))))

(test-->> 
 Baseline-reduction
 (term (· ((λ f (f #t)) ((λ x x) @ (Number? → Number?)))))
 (term (· (((λ f (f #t)) (λ x x)) @ Number?))))

(test-->> 
 Baseline-reduction
 (term (· (λ f ((f (λ x x)) 1) ((λ x x) @ ((Number? → Number?) → (Number? → Number?))))))
 (term (· (λ f ((f (λ x x)) 1) ((λ x x) @ ((Number? → Number?) → (Number? → Number?)))))))
