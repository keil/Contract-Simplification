#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")
;(require "../baseline.rkt")
(require "../old2/old.syntax.rkt")

;; Test Syntax
;; -----------
;; Each source term is either reducible or in a canonical form (non-reducible).

(define (syntax-ok? M) (xor (canonical? M) (reducible? M)))

(define xterm
  (redex-check λCon-Baseline M (syntax-ok? (term M)) #:print? "a" #:attempts 10000000))

;; Manual Test
;; -----------

(define (print-result M) (string-append "canonical? " (format "~a" (canonical? M)) " - " "reducible? " (format "~a" (reducible? M))))

(print-result xterm)