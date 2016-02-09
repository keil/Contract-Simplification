#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(require "examples.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Note: Matthias Keil
; Test are skipped until predicate handling is implemented
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;(test-->> 
; Pre-evaluation
; (term (assert (assert 0 Pos?) Nat?)) (term +blmae))

;(test-->> 
; Pre-evaluation
; (term ((0 @ Nat?) @ Pos?))
; (term +blame))

(test-->>
 Pre-evaluation
 (term ((λ x (+ x 1)) (1 @ Nat?)))
 (term ((λ x (+ x 1)) 1)))

(test-->>
 Pre-evaluation
 (term ((λ x (+ x (1 @ Nat?))) 1))
 (term ((λ x (+ x 1)) 1)))

(test-->>
 Pre-evaluation
 (term ((λ x (+ (x @ Nat?) 1)) 1))
 (term ((λ x (+ (x @ Nat?) 1)) 1)))

(test-->>
 Pre-evaluation
 (term (1 @ (Nat? → Nat?)))
 (term 1))

(test-->>
 Pre-evaluation
 (term (x @ (Nat? → Nat?)))
 (term (x @ (Nat? → Nat?))))

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

(test-->>
 Baseline-reduction
 example:verify/0
 (term ((λ x (+ 1 x)) 1)))

(test-->>
 Baseline-reduction
 example:skip/0
 (term (+ 1 1)))

(test-->>
 Baseline-reduction
 example:skip/1
 (term (+ 1 1)))

(test-->
 Baseline-reduction
 example:unroll/0
 (term ((λ x ((x @ (Num? → Num?)) 2)) (λ x (+ x 1)))))

(test-results)