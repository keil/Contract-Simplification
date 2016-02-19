#lang racket
(require redex)
(require rackunit)

(require "baseline.rkt")


;(check-eq?
; (term (≤ (λ x (< x 0)) (λ x (< x 0))))
; #t)


(redex-match? λCon-Baseline T (term x))
(redex-match? λCon-Baseline T (term 1))
(redex-match? λCon-Baseline T (term (λ x x)))

(redex-match? λCon-Baseline T (term ((λ x 1) 1)))

(redex-match? λCon-Baseline T (term ((λ x (1 @ ♭ Number?)) 1)))





(define canonical? (redex-match λCon-Baseline T))
(define reducible? (redex-match λCon-Baseline Reducible))

(define (syntax-ok? M) (xor (canonical? M) (reducible? M)))

(redex-check λCon-Baseline M (syntax-ok? (term M)) 	#:attempts 1000000 )

;; attempts