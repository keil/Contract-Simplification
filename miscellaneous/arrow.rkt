#lang racket
(require redex)

(provide (all-defined-out))

(define-language arith
  ((e f) v (numerics e ...))
  (numerics + * - /)
  ((u v w) number)
  ((E F) hole (numerics v ... E e ...)))

(define arith-reduction
  (reduction-relation
   arith
   #:domain e
   #:arrow -->
   
   (--> (in-hole E (numerics number ...))
        (in-hole E (δ numerics number ...))
        "δ")
   
   
   ))


(define-extended-language arith2 arith
  (relationals < > = <= >=)
  (logicals and or not)
  
  ((l m n) boolean (relationals e f) (logicals l ...))
  ((L M N) hole (relationals L f) (relationals number L) (logicals boolean ... L e ...)))

(define arith2-reduction
  (extend-reduction-relation 
   arith-reduction
   arith2
   #:domain l
   #:arrow ==>
   
   (==> (in-hole L (logicals boolean ...))
        (in-hole L (δ logicals boolean ...))
        "δ/boolean")
   
   (==> (in-hole L (relationals number ...))
        (in-hole L (δ/ relationals number ...))
        "δ/number")
   
   #| (--> (in-hole E (numerics number ...))
        (in-hole E (δ numerics number ...))
        "δx") |#
   
   (==> (in-hole L (numerics number_0 ...))
        (in-hole L e)
        ;(in-hole L ,(car (apply-reduction-relation* arith-reduction 1)))
        "Base"
        ;(where number (x--> (numerics number_0 ...)))
        
        ;(where v (car (apply-reduction-relation* arith-reduction 1)))
        (where e ,(car (apply-reduction-relation arith-reduction (term (numerics number_0 ...)))))
   )
   
;   with
;   [(==> (in-hole L e) (in-hole L number))
;    (--> e number)]
   
   
   ))

(define-metafunction arith
  x--> : e -> number
  ;[(x--> e) ,(--> (term e))]
  [(x--> e) ,(car (apply-reduction-relation* arith-reduction (term e)))])


;; Delta (δ)
;; ---------
(define namespace (make-base-namespace))

(define-metafunction arith
  δ : numerics number ... -> number
  [(δ numerics number ...) ,(eval (term (numerics number ...)) namespace)])

(define-metafunction arith2
  δ/ : relationals number ... -> boolean
  [(δ/ relationals number ...) ,(eval (term (relationals number ...)) namespace)])












;(traces arith-reduction (term (* (+ 1 2 3) (- 1 2 3))))
;(traces arith2-reduction (term (* (+ 1 2 3) (- 1 2 3))))

;(traces arith2-reduction (term (> 1 2)))
(traces arith2-reduction (term (> (+ 1 2 3) (- 1 2 3))))
