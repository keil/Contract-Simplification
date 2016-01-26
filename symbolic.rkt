#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")

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

(define-extended-language λCon-Symbolic λCon
  
  ;; Predicates (TODO)
  (P (flat (λ x M)) named)
  
  ;; Values
  ((U V W) K (λ x M) ?)
  
  ;; Symbolic Values
  ((S T) (V / (P ...)) (S @ Q))
  
  ;; Terms
  ((L M N) .... S (M || N))
  
  ;; Contexts
  ((E F) hole (E N) (S E) (op S ... E M ...) (if E M N) (E @ C) (S @ (eval E P)) (E || N) (S || E))
  
  ;; False Values
  (false .... ?))


;; Maybe-false
;; ===========
(define maybe-false? 
  (redex-match? λCon-Symbolic false))

;; Maybe-true
;; ==========
(define maybe-true? 
  (not (redex-match? λJ false)))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define-metafunction λCon-Symbolic
  unpack : S -> V
  [(unpack (V / (P ...))) V]
  [(unpack V) V]
  [(unpack any) ?]
  )

(define-metafunction λCon-Symbolic
  δ/ : op V ... -> V
  [(δ/ op K ...) (δ op K ...)]
  [(δ/ op any ...) ?]
  )

(define-metafunction λCon-Symbolic
  Δ : op S ... -> S
  [(Δ numeric (V / (P ...)) ...) ((δ/ numeric V ...) / (Num?))]  
  [(Δ logical (V / (P ...)) ...) ((δ/ logical V ...) / (Bool?))]
  [(Δ relational (V / (P ...)) ...) ((δ/ relational V ...) / (Bool?))]
  [(Δ predicates (V / (P ...)) ...) ((δ/ predicates V ...) / (Bool?))]
  )

(define-metafunction λCon-Symbolic
  ★ : S S -> S
  ;; Merge predicates when values are identival
  [(★ (K / (P_v ...)) (K / (P_w ...))) (K / (⊕ (P_v ...) (P_w ...)))]
  [(★ ((λ x M) / (P_v ...)) ((λ x M) / (P_w ...))) ((λ x M) / (⊕ (P_v ...) (P_w ...)))]
  ;; Make splitted function body
  [(★ ((λ x M) / (P_v ...)) ((λ x N) / (P_w ...))) ((λ x (M || N)) / (⊗ (P_v ...) (P_w ...)))]
  [(★ (V / (P_v ...)) (W / (P_w ...))) (? / (⊗ (P_v ...) (P_w ...)))]
  )

(define-metafunction λCon-Symbolic
  ⊗ : (P ...) (P ...) -> (P ...)
  [(⊗ () (P ...)) ()]
  [(⊗ (P ...) ()) ()]
  [(⊗ (P) (P_0 ... P P_i ...)) (P)]
  [(⊗ (P) (P_0 ...)) (⊤)]  
  [(⊗ (P_0 P_1 ...) (P ...)) (⊕ (⊗ (P_0) (P ...)) (⊗ (P_1 ...) (P ...)))] 
  )

(define-metafunction λCon-Symbolic
  ⊕ : (P ...) (P ...) -> (P ...)  
  [(⊕ (P ...) ()) (P ...)]
  [(⊕ (P_0 ... P_i P_j ...) (P_i P ...)) (⊕ (P_0 ... P_i P_j ...) (P ...))]
  [(⊕ (P ...) (P_0 P_1 ...)) (⊕ (P ... P_0) (P_1 ...))])

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic
   
   ;; Abstraction of Values
   ;; =====================
   
   (--> (in-hole E V)
        (in-hole E (V / (⊤)))
        "Abstract")
   
   (--> (in-hole E (((λ x M) / (P_v ...)) || ((λ y N) / (P_w ...))))
        (in-hole E (((λ z (subst x z M)) / (P_v ...)) || ((λ z (subst x z N)) / (P_w ...))))
        "α"
        (side-condition (not (eq? (term x) (term y))))
        (fresh z))
   
   (--> (in-hole E ((V / (P_v ...)) || (W / (P_w ...))))
        (in-hole E (★ (V / (P_v ...)) (W / (P_w ...))))
        "Join"
        (side-condition
         (nand
          (redex-match? λCon (λ x M) (term V))
          (redex-match? λCon (λ x M) (term W)))))
   
   (--> (in-hole E (((λ x M) / (P_l ...)) || ((λ x N) / (P_r ...))))
        (in-hole E (★ ((λ x M) / (P_l ...)) ((λ x N) / (P_r ...))))
        "Join/Function")
   
   ;; Rules from λJ
   ;; =============
   
   (--> (in-hole E (op S ...))
        (in-hole E (Δ op S ...))
        "Δ")
   
   (--> (in-hole E (((λ x M) / (P ...)) S))
        (in-hole E (subst x S M))
        "β")
   
   (--> (in-hole E ((? / (P ...)) S))
        (in-hole E (? / (⊤)))
        "Β")
   
   (--> (in-hole E (if (V / (P ...)) M N))
        (in-hole E M)
        "If/true"
        (side-condition (not (maybe-false? (term V)))))
   
   (--> (in-hole E (if (V / (P ...)) M N))
        (in-hole E N)
        "If/false"
        (side-condition (false? (term V))))
   
   (--> (in-hole E (if (? / (P ...)) M N))
        (in-hole E (M || N))
        "If/?")  
   
   ;; Rules from λCon
   ;; ===============
   
   ;; Immediate Contracts
   ;; -------------------
   
   (--> (in-hole E ((V / (P_0 ...)) @ (flat M)))
        (in-hole E ((V / (P_0 ...)) @ (eval (M V) (flat M))))
        "Flat")
   
   (--> (in-hole E ((V / (P_0 ...)) @ (eval (W / (P_w ...)) P)))
        (in-hole E (V / (⊕ (P_0 ...) ((pretty P)))))
        "Unit"
        (side-condition (not (false? (term W)))))
   
   (--> (in-hole E ((V / (P_0 ...)) @ (eval (W / (P_w ...)) P)))
        (in-hole E (V / (⊕ (P_0 ...) (⊥))))
        "Blame"
        (side-condition (false? (term W))))
   
   (--> (in-hole E (S @ (C ∪ D)))
        (in-hole E ((S @ C) || (S @ D)))
        "Union")
   
   (--> (in-hole E (S @ (C ∩ D)))
        (in-hole E ((S @ C) || (S @ D)))
        "Intersection")
   
   ;; Delayed Contracts
   ;; -----------------
   
   (--> (in-hole E ((S @ (C → D)) T))
        (in-hole E ((S (T @ C)) @ D))
        "D-Function")
   
   (--> (in-hole E ((S @ (x → C)) T)) 
        (in-hole E ((S T) @ C))
        "D-Dependent") ;; TODO
   
   ;; Miscellaneous
   ;; -------------
   
   (--> (in-hole E ((V / (P ...)) @ named))
        (in-hole E ((V / (P ...)) @ (lookup named)))
        "Lookup")
   
   ))

;; Symbolic Execution
;; ================== 

(define
  (⇓/symbolic M)
  (car (apply-reduction-relation* Symbolic-reduction M)))

(define
  (⇒/symbolic M)
  (⇓/symbolic (term (,M (? / (⊤))))))

(define ;; TODO
  (⇒*/symbolic M)
  (do [(N (⇒/symbolic M) (⇒/symbolic N))] ((not (redex-match? λCon-Symbolic ((λ x M) / (P ...)) N)) N)))




(⇓/symbolic (term ((λ x (+ x 1)) 1)))
(⇓/symbolic (term (((λ x (+ x 1)) @ (Nat? → Nat?)) 1)))
(⇓/symbolic (term ((λ x (+ x 1)) (? / (⊤)))))
        
(⇒/symbolic (term (λ x (+ x 1))))
(⇒/symbolic (term (λ x (λ y (+ x y)))))
(⇒*/symbolic (term (λ x (λ y (+ x y)))))

(⇒*/symbolic (term (λ x (λ y (+ x y)))))
(⇒*/symbolic (term (λ x (λ y (λ z (- (+ x y) z))))))
(⇒*/symbolic (term (λ x (λ y (λ z (and (+ x y) z))))))

(⇓/symbolic (term (if #t 1 2)))

(⇓/symbolic
 (term ((λ x ((if (boolean? x) (λ x (or x 1)) (λ x (+ x 1))) x)) 1))
 )

(⇒*/symbolic
 (term (λ x ((if (boolean? x) (λ x (or x 1)) (λ x (+ x 1))) x))))

;; better to say if fullfilles Bool? or Num?
;; which is another predicate
;(traces Symbolic-reduction  (term ((λ x ((if (boolean? x) (λ x (or x 1)) (λ x (+ x 1))) x)) (? / (⊤)))))


;(traces Symbolic-reduction (term (if #t 1 2)))
