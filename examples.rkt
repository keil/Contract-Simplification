#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(provide (all-defined-out))

#|
 ___               _ _            ___        _         _   _          
| _ ) __ _ ___ ___| (_)_ _  ___  | _ \___ __| |_  _ __| |_(_)___ _ _  
| _ \/ _` (_-</ -_) | | ' \/ -_) |   / -_) _` | || / _|  _| / _ \ ' \ 
|___/\__,_/__/\___|_|_|_||_\___| |_|_\___\__,_|\_,_\__|\__|_\___/_||_|

|#



;; Examples: Simple Terms
;; ======================

(define 
  example:term/0
  (term (((λ x (+ x 1)) @ (Num? → Num?)) 1)))

(define 
  example:term/1
  (term (λ x (((+ x 1) @ Num?) @ Pos?))))

(define 
  example:term/2
  (term ((λ x (((+ x 1) @ Num?) @ Pos?)) 1)))



;; Examples: Remain
;; ================

(define 
  example:remain/0
  (term ((λ x 1) @ (Num? → Num?))))

(define 
  example:remain/1
  (term ((λ x x) @ (Num? → Num?))))

(define 
  example:remain/2
  (term ((λ x x) @ ((Num? → Num?) → (Num? → Num?)))))

(define 
  example:remain/3
  (term ((λ f (f 1)) ((λ x 1) @ (Num? → Num?)))))

(define 
  example:remain/4
  (term ((λ f (f #t)) ((λ x 1) @ (Num? → Num?)))))

(define 
  example:remain/5
  (term ((λ f (f 1)) ((λ x x) @ (Num? → Num?)))))

(define 
  example:remain/6
  (term ((λ f (f #t)) ((λ x x) @ (Num? → Num?)))))

(define 
  example:remain/7
  (term (λ f ((f (λ x x)) 1)  ((λ x x) @ ((Num? → Num?) → (Num? → Num?))))))



;; Examples: Pre-evaluation
;; ========================

(define 
  example:verify/0
  (term ((λ x (+ (1 @ Num?) x)) 1)))

(define 
  example:skip/0
  (term (+ (1 @ (Num? → Num?)) 1)))

(define 
  example:skip/1
  (term ((+ 1 1) @ (Num? → Num?))))

(define 
  example:unroll/0
  (term ((λ x (x 2)) ((λ x (+ x 1)) @ (Num? → Num?)))))



;; Examples: Unroll
;; ================

(define 
  example:unroll/1
  (term ((λ x (+ 1 (x 2))) ((λ x (+ x 1)) @ (Num? → Num?)))))

(define 
  example:unroll/2
  (term ((λ x (+ (x 1) (x 2))) ((λ x (+ x 1)) @ (Num? → Num?)))))



;; Examples: Unfold
;; ================

(define 
  example:unfold/0
  (term ((λ x ((x @ (Num? → Num?)) 2)) (λ x (+ x 1)))))

(define 
  example:unfold/1
  (term ((λ x (+ 1 ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(define 
  example:unfold/2
  (term ((λ x (+ ((x @ (Num? → Num?)) 1) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))