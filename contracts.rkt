#lang racket
(require redex)
(require "lcon.rkt")

(provide (all-defined-out))

#|
  ___         _               _      
 / __|___ _ _| |_ _ _ __ _ __| |_ ___
| (__/ _ \ ' \  _| '_/ _` / _|  _(_-<
 \___\___/_||_\__|_| \__,_\__|\__/__/
                                     
|#

(define Any?
  (term (flat (λ x #t))))

(define None?
  (term (flat (λ x #f))))




(define Num?
  (term (flat (λ x (number? x)))))

(define Str?
  (term (flat (λ x (string? x)))))

(define Bool?
  (term (flat (λ x (boolean? x)))))




(define Pos?
  (term (flat (λ x (> x 0)))))

(define Neg?
  (term (flat (λ x (< x 0)))))

(define Nat?
  (term (flat (λ x (or (> x 0) (= x 0))))))




;; TODO, write nore contracts in the style of
;; http://docs.racket-lang.org/reference/number-types.html