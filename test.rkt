#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "contracts.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

(define
  (reduce M)
  (car (apply-reduction-relation* Baseline-reduction M)))

(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))

(define
  (compare M)
  (eq? (evaluate (reduce M)) (evaluate M)))


;; Test Cases

(define 
  (example-0)
  (term ((λ x (+ x (1 @ ,Nat))) 1)))



(compare example-0)


;;(redex-check λCon M (compare (term M)) #:attempts 100000)
;;(test-results)