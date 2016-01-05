#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "contracts.rkt")

(provide (all-defined-out))

#|
 ___          _                           _ 
/ __|_  _ _ _| |_ __ ___ __  __ _ _ _  __| |
\__ \ || | ' \  _/ _` \ \ / / _` | ' \/ _` |
|___/\_, |_||_\__\__,_/_\_\ \__,_|_||_\__,_|
     |__/                                   
 ___                     _   _      ___                 _         
/ __| ___ _ __  __ _ _ _| |_(_)__  |   \ ___ _ __  __ _(_)_ _  ___
\__ \/ -_) '  \/ _` | ' \  _| / _| | |) / _ \ '  \/ _` | | ' \(_-<
|___/\___|_|_|_\__,_|_||_\__|_\__| |___/\___/_|_|_\__,_|_|_||_/__/
                                                                  
|#

(define-extended-language λCon-Baseline λCon
  
  ;; Baseline Terms (Final Terms)
  ((S T) K x (λ x T) (S T) (op T ...) (x @ I) ((λ x M) @ Q))
           
  ;; Baseline Context
  ((A B) hole (λ x B) (op S ... B M ...) (B M) (S B) (B @ C))
)

;; TODO
;; every  is also an λCon-term



#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define Baseline-reduction
  (reduction-relation
   λCon-Baseline
   
   (--> (in-hole B (V @ I))
        (in-hole B ,(car (apply-reduction-relation* λCon-reduction (term (V @ I)))))
        "Baseline-Flat"
   )
;   (--> (in-hole R (assert v I))
;        (in-hole R ,(car (apply-reduction-relation* λCon-reduction (term (assert v I)))))
;        "Red-2-Flat"
;   )
   (--> (in-hole B (K @ Q))
        (in-hole B K)
        "Baseline-Function"
   )
))

(test-->> Baseline-reduction (term ((1 @ ,Nat) @ ,Nat)) (term 1))
(test-->> Baseline-reduction (term ((0 @ ,Nat) @ ,Nat)) (term 0))

;(test-->> Baseline-reduction (term (assert (assert 0 ,Pos) ,Nat)) (term blmae))
(test-->> Baseline-reduction (term ((0 @ ,Nat) @ ,Pos)) (term blame))

(test-->> Baseline-reduction (term ((λ x (+ x 1)) (1 @ ,Nat))) (term ((λ x (+ x 1)) 1)))
(test-->> Baseline-reduction (term ((λ x (+ x (1 @ ,Nat))) 1)) (term ((λ x (+ x 1)) 1)))

(test-->> Baseline-reduction (term ((λ x (+ (x @ ,Nat) 1)) 1)) (term ((λ x (+ (x @ ,Nat) 1)) 1)))
 
(test-->> Baseline-reduction (term (1 @ (,Nat → ,Nat))) (term 1)) 
(test-->> Baseline-reduction (term (x @ (,Nat → ,Nat))) (term (x @ (,Nat → ,Nat))))

;(traces Baseline-reduction (term (assert (assert 0 ,Pos) ,Nat)))
;(traces Baseline-reduction (term ((λ x (+ x 1)) (assert 1 ,Nat))))
;(traces Baseline-reduction (term ((λ x (+ x (assert 1 ,Nat))) 1)))


(stepper Baseline-reduction (term (assert (assert 0 ,Pos) ,Nat)))

(define done? (redex-match? λCon-Baseline S))

; TODO, other test relation
(done? (term (+ 1 2)))
(done? (term ((λ x (+ x 1)) 1)))
(done? (term (((λ x (+ x 1)) @ (,Nat → ,Nat)) 1)))

(done? (term ((λ x (+ x 1)) (1 @ ,Nat))))
(done? (term ((λ x (+ x (1 @ ,Nat))) 1)))
(done? (term ((λ x (+ (x @ ,Nat) 1)) 1)))


(define
  (reduce M)
  (car (apply-reduction-relation* Baseline-reduction M)))

(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))

(define
  (compare M)
  (eq? (evaluate (reduce M)) (evaluate M)))


(define test (term ((λ x (+ x (1 @ ,Nat))) 1)))

;(traces Baseline-reduction test)
(reduce test)

;(redex-match? λCon-Baseline S test)
;(redex-match? λCon-Baseline S (reduce test))
;(redex-match? λCon M test)
;(redex-match? λCon M (reduce test))

;(done? test)
;(done? (reduce test))


;test
(reduce test)
(evaluate test)
(evaluate (reduce test))
(compare test)



;; check canonicalize
;;(define (step? M) (= (length (apply-reduction-relation canonicalize M)) 1)) 
;;(redex-check Contracts M (if (not (canonical? (term M))) (step? (term M)) #t) #:attempts 100000)

;(redex-check λCon M (compare (term M)) #:attempts 100000)

;; tests
;;(test-results)

