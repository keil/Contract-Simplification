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
  ((S T) (V / P ...) (S @ Q))
  
  ;; Terms
  ((L M N) .... S)
  
  ;; Contexts
  ((E F) hole (E N) (S E) (op S ... E M ...) (If E M N) (E @ C) (S @ (eval E)))
  
  ;; False Values
  (false .... ?))

#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

(define-metafunction λCon-Symbolic
  unpack : S -> V
  [(unpack (V / P ...)) V]
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
  [(Δ numeric (V / P ...) ...) ((δ/ numeric V ...) / Num?)]  
  [(Δ logical (V / P ...) ...) ((δ/ logical V ...) / Bool?)]
  [(Δ relational (V / P ...) ...) ((δ/ relational V ...) / Bool?)]
  [(Δ predicates (V / P ...) ...) ((δ/ predicates V ...) / Bool?)]
  )

(define Symbolic-reduction
  (reduction-relation
   λCon-Symbolic

   ;; Abstraction of Values
   ;; =====================
   
   (--> (in-hole E V)
        (in-hole E (V / ⊤))
        "Abstract")
   
   ;; Rules from λJ
   ;; =============
   
   (--> (in-hole E (op S ...))
        (in-hole E (Δ op S ...))
        "Δ")
   
   (--> (in-hole E (((λ x M) / P ...) S))
        (in-hole E (subst x S M))
        "β")
   
   ;; Application of ? .. (? 1) ?
   
  (--> (in-hole E (if (V / P ...) M N))
        (in-hole E M)
        "if/true"
        (side-condition (not (false? (term V)))))
   
   (--> (in-hole E (if (V / P ...) M N))
        (in-hole E M)
        "if/false"
        (side-condition (false? (term V))))
  
   (--> (in-hole E (if (? / P ...) M N))
        (in-hole E (? / ))
        "if"
   )  
   ;; TODO
   ;; ? is also a false value.
  
   ;; Rules from λCon
   ;; ===============
   
   ;; Immediate Contracts
   ;; -------------------
   
   (--> (in-hole E ((V / P_0 ...) @ (flat M)))
        (in-hole E ((V / P_0 ... (pretty (flat M))) @ (eval (M V))))
        "Flat")

   (--> (in-hole E ((V / P_0 ...) @ (eval (W / P_n ...))))
        (in-hole E (V / P_0 ...))
        "Unit"
        (side-condition (not (false? (term W)))))
   
   (--> (in-hole E ((V / P_0 ... P_n) @ (eval (W / P ...))))
        (in-hole E (V / P_0 ...))
        "Blame"
        (side-condition (false? (term W))))

   (--> (in-hole E (S @ (C ∪ D)))
        (in-hole E ((S @ C) @ D))
        "Union")
   
   (--> (in-hole E (S @ (I ∩ C)))
        (in-hole E ((S @ I) @ C))
        "Intersection")
   
   ;; Delayed Contracts
   ;; -----------------
   
   (--> (in-hole E ((S @ (C → D)) T))
        (in-hole E ((S (T @ C)) @ D))
        "D-Function")
   
   (--> (in-hole E ((S @ (x → C)) T)) 
        (in-hole E ((S T) @ C))
        "D-Dependent") ;; TODO
   
   (--> (in-hole E ((S @ (Q ∩ R)) T))
        (in-hole E (((S @ Q) @ R) T))
        "D-Intersection")
   
   ;; Miscellaneous
   ;; -------------
  
   (--> (in-hole E ((V / P ...) @ named))
        (in-hole E ((V / P ...) @ (lookup named)))
        "Lookup")
   
   ))

;; Symbolic Execution
;; ================== 

(define
  (analyse M)
  (car (apply-reduction-relation* Symbolic-reduction M)))