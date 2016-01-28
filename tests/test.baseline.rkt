#lang racket
(require redex)

(require "../baseline.rkt")
(require "../examples/examples.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#



;; Test: Simple Terms
;; ==================

(test-->>
 Baseline-reduction
 example:term/0
 (term (((λ x (+ x 1)) 1) @ Num?)))

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Note: Matthias Keil
; [Lower] moves only function/delayed contracts
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;(test-->>
; Baseline-reduction
; example:term/1
; (term (((λ x (+ x 1)) @ (⊤ → Num?)) @ (⊤ → Pos?))))
 
;(test-->>
; Baseline-reduction
; example:term/2
; (term ((((λ x (+ x 1)) 1) @ Num?) @ Pos?)))



;; Test Unroll
;; ===========

(test-->
 Baseline-reduction
 example:unroll/1
 (term ((λ x (+ 1 ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction
 example:unroll/2
 (term ((λ x (+ ((x @ (Num? → Num?)) 1) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction
 example:unfold/0
 (term ((λ x ((x (2 @ Num?)) @ Num?)) (λ x (+ x 1)))))



;; Test: Unfold
;; ============

(test-->
 Baseline-reduction
 example:unfold/1
  (term ((λ x (+ 1 ((x (2 @ Num?)) @ Num?))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction
 example:unfold/2
 (term ((λ x (+ ((x (1 @ Num?)) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction
 (term ((λ x (+ ((x (1 @ Num?)) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1))))
 (term ((λ x (+ ((x 1) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction
 (term ((λ x (+ ((x 1) @ Num?) ((x @ (Num? → Num?)) 2))) (λ x (+ x 1))))
 (term ((λ x (+ ((x 1) @ Num?) ((x (2 @ Num?)) @ Num?))) (λ x (+ x 1)))))

(test-->
 Baseline-reduction
 (term ((λ x (+ ((x 1) @ Num?) ((x (2 @ Num?)) @ Num?))) (λ x (+ x 1))))
 (term ((λ x (+ ((x 1) @ Num?) ((x 2) @ Num?))) (λ x (+ x 1)))))

(test-results)