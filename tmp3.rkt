#lang racket
(require redex)
(require rackunit)

(require "baseline.rkt")


;(check-eq?
; (term (≤ (λ x (< x 0)) (λ x (< x 0))))
; #t)


;(redex-match? λCon-Baseline T (term x))
;(redex-match? λCon-Baseline T (term 1))
;(redex-match? λCon-Baseline T (term (λ x x)))

;(redex-match? λCon-Baseline T (term ((λ x 1) 1)))

;(redex-match? λCon-Baseline T (term ((λ x (1 @ ♭ Number?)) 1)))



 

(define canonical? (redex-match λCon-Baseline T))
(define reducible? (redex-match λCon-Baseline Reducible))
;; xor ?
(define (syntax-ok? M) (xor (canonical? M) (reducible? M)))

(redex-check λCon-Baseline M (syntax-ok? (term M)) #:print? "a"	#:attempts 10000000 )

(define (print-result M) (string-append "canonical? " (format "~a" (canonical? M)) " - " "reducible? " (format "~a" (reducible? M))))

(print-result (term ((#t @ ιZ (hg ↦ (-blame ♭k))) @ ιNegative? (((flat #t) ∩ ⊤) ∩ (⊥ ∪ ⊤)))))
(print-result (term ((λ zCXu fB) @ ιOdZ ((⊤ ∩ ⊤) ∩ ((⊤ ∪ ⊤) ∪ ⊤)))))

;; attempts



#|

blame?

v @ ⊥ must be reduces as other contracts cannot ptoceed
v @ T when lifting is not usefull, as T gets reduced

|#