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

;(define 
;  (done? S)
;  (redex-match? λCon-Baseline S))

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
   (--> (in-hole B (K @ Q))
        (in-hole B K)
        "Baseline-Function"
   )
))