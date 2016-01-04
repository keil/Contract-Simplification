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
  ((u v w) .... blame ((λ x e) @ C))
  
  ;; expressions
  ((e f g) .... (e @ C))
  
  ;; evaluation context
  ((E F G) .... (E @ C) (v @ (eval E)))
)

;; test 
(redex-match λ_C e (term (1 @ (flat 1))))
(redex-match λ_C e (term ((+ 1 2) @ (flat 1))))

(redex-match λ_C e (term (1 @ (flat (+ 1 1)))))
(redex-match λ_C e (term ((+ 1 2) @ (flat (+ 1 1)))))

(redex-match λ_C e (term ((λ x (+ x 1)) @ ((flat 1) → (flat 1)))))
(redex-match λ_C e (term (((λ x (+ x 1)) @ ((flat 1) → (flat 1))) 1)))

;; _____          _            _   _             
;;|  __ \        | |          | | (_)            
;;| |__) |___  __| |_   _  ___| |_ _  ___  _ __  
;;|  _  // _ \/ _` | | | |/ __| __| |/ _ \| '_ \ 
;;| | \ \  __/ (_| | |_| | (__| |_| | (_) | | | |
;;|_|  \_\___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

(define λ_C-reduction
  (extend-reduction-relation λ_J-reduction
   λ_C   (--> (in-hole E (v @ (flat e)))
        (in-hole E (v @ (eval (e v))))
        "Flat"
   )
   (--> (in-hole E (v @ (eval 1)))
        (in-hole E v)
        "Unit"
   )
   (--> (in-hole E (v @ (eval 0)))
        (in-hole E blame)
        "Blame"
   )
   
   (--> (in-hole E ((v @ (C → D)) w))
        (in-hole E ((v (w @ C)) @ D))
        "Function"
   )
))

;;; extebd dubst

; contracts
(define true (term (flat (λ x 1))))
(define false (term (flat (λ x 0))))

;; test 
;(traces λ_C-reduction (term ((+ 1 2) @ (flat (λ x 1)))))
;(traces λ_C-reduction (term ((+ 1 2) @ ,false)))

(traces λ_C-reduction (term (((λ x 1) @ (,true → ,true)) 1)))


;(traces let-reduction (term xx))
;(traces let-reduction (term (1 + 2)))
;(traces let-reduction (term xx))
;(traces let-reduction (term (let x = (1 + 1) in (2 + 2))))
;(traces let-reduction (term (let x = (1 + 1) in (x + 2))))
;(traces let-reduction (term (let x = 1 in (x + 1))))
;(traces let-reduction (term (let x = 1 in (let y = 1 in (x + y)))))