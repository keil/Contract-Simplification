#lang racket
(require redex)

(require "lj.rkt")

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

(define-extended-language λCon λJ
  
  ;; Contracts
  ((C D) I Q (C ∪ D) (I ∩ C))
  
  ; Immediate Contracts
  ((I J) (flat (λ x M)))

  ; Delayed Contracts
  ((Q R) (C → D) (Q ∩ R)) ;; (x → C)
  
  ;; Values
  ((U V W) .... ((λ x M) @ Q))
  
  ;; Terms
  ((L M N) .... (M @ C))
  
  ;; Contexts
  ((E F) .... (E @ C) (V @ (eval E)))
  
  ;; False Value
  (false #f 0 "")
)

(define false? 
  (redex-match? λCon false))

(define λCon-value?
  (redex-match? λCon V))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define λCon-reduction
  (extend-reduction-relation λJ-reduction
   λCon
;   (--> (in-hole E (assert v C))
;        (in-hole E (v @ C))
;        "Assert"
;   )
   ;; Immediate Contarcts
   (--> (in-hole E (V @ (flat M)))
        (in-hole E (V @ (eval (M V))))
        "Flat"
   )
   (--> (in-hole E (V @ (eval W)))
        (in-hole E V)
        "Unit"
        (side-condition (not (false? (term W))))
        ;(side-condition (not (equal? (term W) #f)))
   )
   (--> (in-hole E (V @ (eval W)))
        blame ;; TODO, Change to V and introduce top-level blame
        "Blame"
        (side-condition (false? (term W)))
   )
   (--> (in-hole E (V @ (C ∪ D)))
        (in-hole E ((V @ C) @ D))
        "Union"
   )
   (--> (in-hole E (V @ (I ∩ C)))
        (in-hole E ((V @ I) @ C))
        "Intersection"
   )
   ;; Delayed Contarcts
   (--> (in-hole E ((V @ (C → D)) W))
        (in-hole E ((V (W @ C)) @ D))
        "D-Function"
   )
   (--> (in-hole E ((V @ (x → C)) W)) ;; TODO
        (in-hole E ((V W) @ C))
        "D-Dependent"
   )
   (--> (in-hole E ((V @ (Q ∩ R)) W))
        (in-hole E (((V @ Q) @ R) W))
        "D-Intersection"
   )
))