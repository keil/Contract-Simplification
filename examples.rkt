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

;; Example: Verify 0
;; -----------------

(define 
  example:verify/0
  (term ((λ x (+ (1 @ Num?) x)) 1)))

(test-->
 Baseline-reduction2
 example:verify/0
 (term ((λ x (+ 1 x)) 1)))

;; Example: Skip 0
;; ---------------

(define 
  example:skip/0
  (term (+ (1 @ (Num? → Num?)) 1)))

(test-->
 Baseline-reduction2
 example:skip/0
 (term (+ 1 1)))

;; Example: Skip 1
;; ---------------

(define 
  example:skip/1
  (term ((+ 1 1) @ (Num? → Num?))))

(test-->
 Baseline-reduction2
 example:skip/1
 (term (+ 1 1)))



;; Example: unroll 0
;; -----------------

(define 
  example:unroll/0
  (term ((λ x (x 2)) ((λ x (+ x 1)) @ (Num? → Num?)))))

(test-->
 Baseline-reduction2
 example:unroll/0
 (term ((λ x ((x @ (Num? → Num?)) 2)) (λ x (+ x 1)))))

;; Example: unroll 1
;; -----------------

(define 
  example:unroll/1
  (term ((λ x (+ 1 (x 2))) ((λ x (+ x 1)) @ (Num? → Num?)))))

(test-->
 Baseline-reduction2
 example:unroll/1
 (term ((λ x (+ 1 ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

;; Example: unroll 2
;; -----------------

(define 
  example:unroll/2
  (term ((λ x (+ (x 1) (x 2))) ((λ x (+ x 1)) @ (Num? → Num?)))))

(test-->
 Baseline-reduction2
 example:unroll/2
 (term ((λ x (+ ((x @ (Num? → Num?)) 1) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))


;; Example: unfold 0
;; -----------------

(define 
  example:unfold/0
  (term ((λ x ((x @ (Num? → Num?)) 2)) (λ x (+ x 1)))))

(test-->
 Baseline-reduction2
 example:unfold/0
 (term ((λ x ((x (2 @ Num?)) @ Num?)) (λ x (+ x 1)))))

;; Example: unfold 1
;; -----------------

(define 
  example:unfold/1
  (term ((λ x (+ 1 ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction2
 example:unfold/1
  (term ((λ x (+ 1 ((x (2 @ Num?)) @ Num?))) (λ x (+ x 1)))))

;; Example: unfold 2
;; -----------------

(define 
  example:unfold/2
  (term ((λ x (+ ((x @ (Num? → Num?)) 1) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction2
 example:unfold/2
 (term ((λ x (+ ((x (1 @ Num?)) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction2
 (term ((λ x (+ ((x (1 @ Num?)) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1))))
 (term ((λ x (+ ((x 1) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction2
 (term ((λ x (+ ((x 1) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1))))
 (term ((λ x (+ ((x 1) @ Num?) ((x (2 @ Num?)) @ Num?))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction2
 (term ((λ x (+ ((x 1) @ Num?) ((x (2 @ Num?)) @ Num?))) (λ x (+ x 1))))
 (term ((λ x (+ ((x 1) @ Num?) ((x 2) @ Num?))) (λ x (+ x 1)))))

(traces Baseline-reduction2
 example:unroll/2)

;; Example: Term-0
;; ---------------

(define 
  example:term/0
  (term (((λ x (+ x 1)) @ (Num? → Num?)) 1)))

(test-->>
 Baseline-reduction2
 example:term/0
 (term (((λ x (+ x 1)) 1) @ Num?)))


;; Example: Term-1
;; ---------------

(define 
  example:term/1
  (term (λ x (((+ x 1) @ Num?) @ Pos?))))

(test-->>
 Baseline-reduction2
 example:term/1
 (term (((λ x (+ x 1)) @ (⊤ → Num?)) @ (⊤ → Pos?))))


;; Example: Term-2
;; ---------------

(define 
  example:term/2
  (term ((λ x (((+ x 1) @ Num?) @ Pos?)) 1)))

(test-->>
 Baseline-reduction2
 example:term/2
 (term ((((λ x (+ x 1)) 1) @ Num?) @ Pos?)))


(traces Baseline-reduction2  example:term/2)

;(done? (term (λ x (((+ x 1) @ ,Num?) @ , Pos?))))

;(traces Baseline-reduction2  (term (λ x (((+ x 1) @ ,Num?) @ , Pos?))))


;; test with application







(test-results)