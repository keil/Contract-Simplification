#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

(require "contracts.rkt")

(provide (all-defined-out))

#|
 ___               _ _            ___        _         _   _          
| _ ) __ _ ___ ___| (_)_ _  ___  | _ \___ __| |_  _ __| |_(_)___ _ _  
| _ \/ _` (_-</ -_) | | ' \/ -_) |   / -_) _` | || / _|  _| / _ \ ' \ 
|___/\__,_/__/\___|_|_|_||_\___| |_|_\___\__,_|\_,_\__|\__|_\___/_||_|

|#



;; Test Cases

(define 
  example-0
  (term ((λ x (+ x (1 @ ,Nat?))) 1)))

;;(compare example-0)


(define 
  example-1
  (term ((λ f (f 1)) ((λ x (+ x 1)) @ (,Num? → ,Num?)))))

;; Contract at different prosiitions
;; on plus, as contract on the outer function
;; and with or without concrete top-level application

(define 
  example-addOne1
  (term 
   ((λ plus (λ x ((plus 1) x))) ((λ x (λ y (+ x y))) @ (,Num? → (,Num? → ,Num?))))))

(define 
  example-addOne2
  (term 
   (((λ plus (λ x ((plus 1) x))) @ ((,Num? → (,Num? → ,Num?)) → (,Num? → ,Num?))) (λ x (λ y (+ x y))))))




