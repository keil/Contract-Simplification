#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")
(require "../baseline.rkt")


;; Manual Test
;; -----------

(define (print-result M) (string-append "canonical? " (format "~a" (canonical? M)) " - " "reducible? " (format "~a" (reducible? M))))

(print-result (term
(((λ yF #t) @ ιD ⊥) ((if feven? xh #t) @ ιF (⊤ → ⊤)))
 ))

;; Test Syntax
;; -----------
;; Each source term is either reducible or in a canonical form (non-reducible).

(define (syntax-ok? M) (xor (canonical? M) (reducible? M)))
(redex-check λCon M (syntax-ok? (term M)) #:print? "a" #:attempts 10000000)







;(redex-match λCon-Baseline (M @ ι (I ∩ C)) (term (yX @ ιw ((flat (string-append)) ∩ ((Natural? ∩ Natural?) ∪ Integer?)))))

;(redex-match? λCon-Baseline (M @ ι (I ∩ C)) (term (yX @ ιw ((flat (string-append)) ∩ ((Natural? ∩ Natural?) ∪ Integer?)))))

;(redex-match? λCon-Baseline Reducible (term (yX @ ιw ((flat (string-append)) ∩ ((Natural? ∩ Natural?) ∪ Integer?)))))


;(redex-match? λCon-Baseline Reducible (term (x @ ι0 (Natural? ∩ Integer?))))
;(redex-match? λCon-Baseline T (term (x @ ι0 (Natural? ∩ Integer?))))


;(reducible? (term (x @ ι0 (Natural? ∩ Integer?))))
;(canonical? (term (term (yX @ ιw ((flat (string-append)) ∩ ((Natural? ∩ Natural?) ∪ Integer?))))))