#lang racket
(require redex)
(require "lj.rkt")

;;  _____             _                               _ 
;; / ____|           | |                             | |
;;| (___  _   _ _ __ | |_ __ ___  __   __ _ _ __   __| |
;; \___ \| | | | '_ \| __/ _` \ \/ /  / _` | '_ \ / _` |
;; ____) | |_| | | | | || (_| |>  <  | (_| | | | | (_| |
;;|_____/ \__, |_| |_|\__\__,_/_/\_\  \__,_|_| |_|\__,_|
;;         __/ |                                        
;;        |___/                                         
;;  _____                            _   _            _                       _           
;; / ____|                          | | (_)          | |                     (_)          
;;| (___   ___ _ __ ___   __ _ _ __ | |_ _  ___    __| | ___  _ __ ___   __ _ _ _ __  ___ 
;; \___ \ / _ \ '_ ` _ \ / _` | '_ \| __| |/ __|  / _` |/ _ \| '_ ` _ \ / _` | | '_ \/ __|
;; ____) |  __/ | | | | | (_| | | | | |_| | (__  | (_| | (_) | | | | | | (_| | | | | \__ \
;;|_____/ \___|_| |_| |_|\__,_|_| |_|\__|_|\___|  \__,_|\___/|_| |_| |_|\__,_|_|_| |_|___/

(define-extended-language λ_C λ_J
  
  ;; Immediate Contracts
  ((I J) (flat e)) 
  
  ; Delayed Contracts
  ((Q R) (C → D))
  
  ;; Contracts
  ((C D) I Q)
  
  ;; values
  ((u v w) .... ((λ x e) @ Q) blame)
  
  ;; expressions
  ((e f g) .... (assert e C))
  
  ;; evaluation context
  ((E F G) .... (assert E C) (E @ C) (v @ (eval E)))
)

;; test 
;(redex-match λ_C e (term (assert 1 (flat 1))))
;(redex-match λ_C e (term (assert (+ 1 2) (flat 1))))

;(redex-match λ_C e (term (assert 1 (flat (+ 1 1)))))
;(redex-match λ_C e (term (assert (+ 1 2) (flat (+ 1 1)))))

;(redex-match λ_C e (term (assert (λ x (+ x 1)) ((flat 1) → (flat 1)))))
;(redex-match λ_C e (term ((assert (λ x (+ x 1)) ((flat 1) → (flat 1))) 1)))

;(redex-match λ_C e (term ((λ x 1) blame)))

;; _____          _            _   _             
;;|  __ \        | |          | | (_)            
;;| |__) |___  __| |_   _  ___| |_ _  ___  _ __  
;;|  _  // _ \/ _` | | | |/ __| __| |/ _ \| '_ \ 
;;| | \ \  __/ (_| | |_| | (__| |_| | (_) | | | |
;;|_|  \_\___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

(define λ_C-reduction
  (extend-reduction-relation λ_J-reduction
   λ_C
   (--> (in-hole E (assert v C))
        (in-hole E (v @ C))
        "Assert"
   )
   (--> (in-hole E (v @ (flat e)))
        (in-hole E (v @ (eval (e v))))
        "Flat"
   )
   (--> (in-hole E (v @ (eval w)))
        (in-hole E v)
        "Unit"
        (side-condition (> (term w) 0))
   )
   (--> (in-hole E (v @ (eval 0)))
        blame
        "Blame"
   )
   (--> (in-hole E ((v @ (C → D)) w))
        (in-hole E ((v (w @ C)) @ D))
        "Function"
   )
))

;;; extebd dubst

; contracts
(define Any (term (flat (λ x 1))))
(define Blame (term (flat (λ x 0))))

(define Pos (term (flat (λ x (> x 0)))))
(define Nat (term (flat (λ x (+ (> x 0) (= x 0))))))

;; test 
;(traces λ_C-reduction (term ((+ 1 2) @ (flat (λ x 1)))))
;(traces λ_C-reduction (term ((+ 1 2) @ ,Any)))

;(traces λ_C-reduction (term ((assert (λ x (+ x 1)) (,Nat → ,Nat)) 1)))

;(traces λ_C-reduction (term ((assert (λ x (+ x 1)) (,Pos → ,Pos)) 0)))
;(traces λ_C-reduction (term ((assert (λ x (- x 1)) (,Pos → ,Pos)) 1)))

;(traces λ_C-reduction (term (((assert (λ x (λ y (+ x y))) (,Pos → (,Pos → ,Pos))) 1) 1)))

;(traces λ_C-reduction (term ((λ f (f 1)) (assert (λ x (+ x 1)) (,Pos → ,Pos)))))

(traces λ_C-reduction (term (
                             (
                              (assert 
                               (λ plus (λ x ((plus x) 1)))
                               ((,Pos → (,Pos → ,Pos)) → (,Pos → ,Pos))
                              )                            
                             (λ x (λ y (+ x y)))
                             ) 1)
                             ))