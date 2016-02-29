#lang racket
#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")
 

;(define canonical? (redex-match λCon-Baseline T))
;(define reducible? (redex-match λCon-Baseline Reducible))

;; xor ?


;; Test Syntax
;; -----------
;; Each source term is either reducible or in a canonical form (non-reducible).

(define (syntax-ok? M) (xor (canonical? M) (reducible? M)))

(redex-check λCon-Baseline M (syntax-ok? (term M)) #:print? "a"	#:attempts 10000000 )

;; Manual Test
;; -----------

(define (print-result M) (string-append "canonical? " (format "~a" (canonical? M)) " - " "reducible? " (format "~a" (reducible? M))))