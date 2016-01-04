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
  (op + * - / < > =)
  
  ;; Values
  ((u v w) c (λ x e))
  
  ;; Expressions
  ((e f g)
   v
   x
   (op e f)
   (e f)
  )
      
  ;; Evaluation Context
  ((E F G)
   hole
   (op E f)
   (op v F)
   (E f)
   (v F)
  )
)

;; _____          _            _   _             
;;|  __ \        | |          | | (_)            
;;| |__) |___  __| |_   _  ___| |_ _  ___  _ __  
;;|  _  // _ \/ _` | | | |/ __| __| |/ _ \| '_ \ 
;;| | \ \  __/ (_| | |_| | (__| |_| | (_) | | | |
;;|_|  \_\___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

;(define-metafunction λ_J
;  subst : x any any -> any
;  [(subst x any x) any]
;  [(subst x any y) y]
;  [(subst x any c) c]
;  [(subst x any (op e f)) (op (subst x any e) (subst x any f))]
;  [(subst x any (λ x e)) (λ x e)]
;  [(subst x any (λ y e)) (λ y (subst x any e))]
;  [(subst x any (e f)) ((subst x any e) (subst x any f))]
;)

(define-metafunction λ_J
  subst : x any any -> any
  [(subst x any_1 (λ x any_2)) (λ x any_2)]
  [(subst x any_1 (λ y any_2))  (λ y (subst x any_1 any_2))]
  [(subst x any x) any]
  [(subst x any y) y]
  [(subst x any_1 (any_2 ...)) ((subst x any_1 any_2) ...)]
  [(subst x any_1 any_2) any_2])

(define-metafunction λ_J
  δ : e -> u
  [(δ (+ v w)) ,(+ (term v) (term w))]
  [(δ (* v w)) ,(* (term w) (term v))]
  [(δ (- v w)) ,(- (term w) (term v))]
  [(δ (/ v w)) ,(/ (term w) (term v))]
  [(δ (< v w)) ,(if (< (term v) (term w)) (term 1) (term 0))]
  [(δ (> v w)) ,(if (> (term v) (term w)) (term 1) (term 0))]
  [(δ (= v w)) ,(if (= (term v) (term w)) (term 1) (term 0))]
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

(provide λ_J)
(provide λ_J-reduction)