#lang racket
(require redex)

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

(define-language λ_J
  
  ;; Constrants 
  (c natural)
    
  ;; Variables
  ((x y z) variable-not-otherwise-mentioned this proto)
  
  ;; Operations
  (op + *)
  
  ;; Expressions
  ((e f g)
   c
   x
   (op e f)
   (λ x e)
   (e f)
  )
 
  ;; Values
  ((u v w) c (λ x e))
      
  ;; Evaluation Context
  ((E F G)
   hole
   (op E f)
   (op v F)
   (E f)
   (v F)
  )
)

;; test
;(redex-match λ_J e (term 1))
;(redex-match λ_J e (term x))

;(redex-match λ_J e (term (+ 1 1)))
;(redex-match λ_J e (term (* 1 1)))

;(redex-match λ_J e (term ((λ x (+ x 1)) 1)))
;(redex-match λ_J e (term (((λ x (λ y (+ x y))) 1) 1)))
;(redex-match λ_J e (term ((λ x (x 1)) (λ x x))))

;; _____          _            _   _             
;;|  __ \        | |          | | (_)            
;;| |__) |___  __| |_   _  ___| |_ _  ___  _ __  
;;|  _  // _ \/ _` | | | |/ __| __| |/ _ \| '_ \ 
;;| | \ \  __/ (_| | |_| | (__| |_| | (_) | | | |
;;|_|  \_\___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

(define-metafunction λ_J
  subst : x v e -> e
  [(subst x v x) v]
  [(subst x v y) y]
  [(subst x v c) c]
  [(subst x v (op e f)) (op (subst x v e) (subst x v f))]
  [(subst x v (λ x e)) (λ x e)]
  [(subst x v (λ y e)) (λ y (subst x v e))]
  [(subst x v (e f)) ((subst x v e) (subst x v f))]
)

(define-metafunction λ_J
  δ : e -> u
  [(δ (+ v w)) ,(+ (term v) (term w))]
  [(δ (* v w)) ,(* (term w) (term v))]
)

(define λ_J-reduction
  (reduction-relation
   λ_J
   (--> (in-hole E (op v w))
        (in-hole E (δ (op v w)))
        "δ"
   )
   (--> (in-hole E ((λ x e) v))
        (in-hole E (subst x v e))
        "App"
   )
))

;; test
(traces λ_J-reduction (term 1))
(traces λ_J-reduction (term x))

(traces λ_J-reduction (term (+ 1 1)))
(traces λ_J-reduction (term (* 1 1)))

(traces λ_J-reduction (term (λ x 1)))
(traces λ_J-reduction (term ((λ x 1) 1)))

(traces λ_J-reduction (term ((λ x (+ x 1)) 1)))
(traces λ_J-reduction (term (((λ x (λ y (+ x y))) 1) 1)))
(traces λ_J-reduction (term ((λ x (x 1)) (λ x x))))