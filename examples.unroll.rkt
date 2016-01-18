#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")

(provide (all-defined-out))

#|
                   _ _           _     _        _   
 _  _ _ _  _ _ ___| | |  ____  _| |__ (_)___ __| |_ 
| || | ' \| '_/ _ \ | | (_-< || | '_ \| / -_) _|  _|
 \_,_|_||_|_| \___/_|_| /__/\_,_|_.__// \___\__|\__|
                                    |__/            
|#

; make it with \ x x and nested contracts

;; this example open another interesting question:
;; When writing the term ((λ x 1) @ (Num? → Num?)) a developer
;; restricts the domain to number values.
;; But, baseline reduction reduces the contract. because the arument
;; is never used. Thus, the initial semantics changed because the contract is
;; removed.
;; e.g. say that I on the domain are not part of the canonical contrats

(define 
  example:unroll/subject/0
  (term ((λ f (f 1)) ((λ x 1) @ (Num? → Num?)))))

(test-->
 Baseline-reduction2
 example:unroll/subject/0
 (term ((λ f (f 1)) (λ x 1))))

;(traces Baseline-reduction2 example:unroll/subject/0)



(define 
  example:unroll/subject/1
  (term ((λ x (x 1)) ((λ x x) @ (Num? → Num?)))))

(test-->
 Baseline-reduction2
 example:unroll/subject/1
 (term ((λ x (x 1)) (λ x 1))))

;(traces Baseline-reduction2 example:unroll/subject/1)







(define 
  example:unroll/subject/2
  (term ((λ f (f 1)) ((λ x x) @ ((Num? → Num?) → (Num? → Num?))))))

(test-->
 Baseline-reduction2
 example:unroll/subject/2
 (term ((λ x (x 1)) (λ x 1))))

(traces Baseline-reduction2 example:unroll/subject/2)



;(define 
;  example:unroll/subject/0
;  (term ((λ f (f 1)) ((λ x (+ x 1)) @ (Num? → Num?)))))







#|
                   _ _              _           _   
 _  _ _ _  _ _ ___| | |  __ ___ _ _| |_ _____ _| |_ 
| || | ' \| '_/ _ \ | | / _/ _ \ ' \  _/ -_) \ /  _|
 \_,_|_||_|_| \___/_|_| \__\___/_||_\__\___/_\_\\__|
                                                    
|#