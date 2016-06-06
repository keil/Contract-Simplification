#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "subset.rkt")

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

(define-extended-language λCon-Join λCon-Subset

  ;; Source Terms
  (S K x (λ x T) (T T) (op T ...) (if T ...) (blame ♭))
  ;; Terms with contarcts
  (T S (T @ ι C))

  ;; Trace
  (Trace T (Trace_l ∥ Trace_r))
  
  ;; Traces
  (Traces hole (Traces ∥ Trace) (T ∥ Traces))
  
  ((F G H)
   hole (λ x F) (F M) (M F) (op M ... F N ...) (if M ... F N ...) (F0 @ b C))
  
  ((F0 G0 H0) 
   (λ x F) (F M) (M F) (op M ... F N ...) (if M ... F N ...) (F0 @ b C))
  

  
  )



(define-metafunction λCon-Join
  ≈/Ctx : G H -> boolean
  
  ;; todo, short
  [(≈/Ctx any any) #t]
  
  [(≈/Ctx hole hole) #t]
  
  [(≈/Ctx (λ x G) (λ x H)) (≈/Ctx G H)]
  
  [(≈/Ctx (op T ... G M_n ...) (op T ... H N_n ...)) ,(and
                                                       (term (≈/Ctx G H))
                                                       (term (≈/Terms (M_n ...) (N_n ...))))]
  
  [(≈/Ctx (if T ... G M_n ...) (op T ... H N_n ...)) ,(and
                                                       (term (≈/Ctx G H))
                                                       (term (≈/Terms (M_n ...) (N_n ...))))]
  
  [(≈/Ctx (G M) (H N)) ,(and (term (≈/Ctx G H)) (term (≈/Term M N)))]
  [(≈/Ctx (T G) (T H)) ,(term (≈/Ctx G H))]
  
  [(≈/Ctx (G @ ι C) H) (≈/Ctx G H)]
  [(≈/Ctx G (H @ ι C)) (≈/Ctx G H)]  
  
  [(≈/Ctx any ... ) #f])


(define-metafunction λCon-Join
  ≈/Terms : (M ...) (N ...) -> boolean
  
  ;; todo, short
  [(≈/Terms any any) #t]
  
  [(≈/Terms (M_0 M_n ...) (N_0 N_n ...)) ,(and
                                           (term (≈/Term M_0 N_0))
                                           (term (≈/Terms (M_n ...) (N_n ...))))]
  [(≈/Terms any ... ) #f])

(define-metafunction λCon-Join
  ≈/Term : M N -> boolean
  
  [(≈/Term K K) #t]
  [(≈/Term x x) #t]
  
  ;; todo, short
  [(≈/Term any any) #t]
  
  [(≈/Term (blame ♭) N) #t]
  [(≈/Term M (blame ♭)) #t]
  
  [(≈/Term (λ x M) (λ x N)) (≈/Term M N)]
  
  [(≈/Term (op M_0 M_n ...) (op N_0 N_n ...)) ,(and
                                                (term (≈/Term M_0 N_0))
                                                (term (≈/Terms (M_n ...) (N_n ...))))]
  
  [(≈/Term (if M_0 M_n ...) (if N_0 N_n ...)) ,(and
                                                (term (≈/Term M_0 N_0))
                                                (term (≈/Terms (M_n ...) (N_n ...))))]  
  
  [(≈/Term (M_0 M_1) (N_0 N_1)) ,(and (term (≈/Term M_0 N_0)) (term (≈/Term M_1 N_1)))]
  
  
  ;[(≈/Term (M @ ι C) (N @ ι C)) (≈/Term M N)]
  [(≈/Term (M @ ι C) N) (≈/Term M N)]
  [(≈/Term M (N @ ι C)) (≈/Term M N)]  
  
  [(≈/Term any ... ) #f])


#|
 ___        _         _   _          
| _ \___ __| |_  _ __| |_(_)___ _ _  
|   / -_) _` | || / _|  _| / _ \ ' \ 
|_|_\___\__,_|\_,_\__|\__|_\___/_||_|
                                     
|#

;; Join-reduction
;; ==================
;; Join splitted observations.

(define Join-reduction
  (reduction-relation
   λCon-Join
   #:domain (ς any)
   
   (--> (ς
         (in-hole Traces (M ∥ M))) 
        (ς
         (in-hole Traces M)) 
        "Join")
   
   
      (--> (ς
         (in-hole Traces ((in-hole G (in-hole ACtx_l S_l))
                         ∥
                         (in-hole H (in-hole ACtx_r S_r)))))
        (ς
         (in-hole Traces ((in-hole G (in-hole (⊔/ACtx ACtx_l ACtx_r) S_l))
                         ∥
                         (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_r)))))
        "Join/Context/XXX"
        (side-condition (term (≈/Ctx G H)))
        (side-condition (term (≈/Term S_l S_r)))
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r))))
        )
 #|  
   (--> (ς
         (in-hole Traces ((in-hole G (in-hole ACtx_l S))
                         ∥
                         (in-hole H (in-hole ACtx_r S)))))
        (ς
         (in-hole Traces ((in-hole G (in-hole (⊔/ACtx ACtx_l ACtx_r) S))
                         ∥
                         (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S)))))
        "Join/Context"
        (side-condition (term (≈/Ctx G H)))
        ;(side-condition (term (≈/Term S_l S_r)))
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r))))
        )
   |#
   
   (--> (ς
         (in-hole Traces ((in-hole G (in-hole ACtx S_l))
                         ∥
                         (in-hole H (in-hole ACtx S_r)))))
        (ς
         (in-hole Traces ((in-hole G (in-hole ACtx (⊔/Term S_l S_r)))
                         ∥
                         (in-hole H (in-hole ACtx (⊔/Term S_r S_l))))))
        "Join/Term"
        (side-condition (term (≈/Ctx G H)))
        (side-condition 
         (or 
          (redex-match? λCon-Subset (blame ♭) (term S_l))
          (redex-match? λCon-Subset (blame ♭) (term S_r))
          )
         )
        ;(side-condition (term (≈ G H)))
        ;(side-condition (term (≈ S_l S_r)))
        ;(side-condition (term (≡/ACtx ACtx_l ACtx_r))))
        )
   
   #|

(--> (ς
         (in-hole F ((in-hole G (in-hole ACtx_l S_l))
                     ∥
                     (in-hole H (in-hole ACtx_r S_r)))))
        (ς
         (in-hole F ((in-hole G (in-hole (⊔/ACtx ACtx_l ACtx_r) S_l))
                     ∥
                     (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_r)))))
        "Join/Context"
        (side-condition (term (≈ G H)))
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r))))
        )
   (--> (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_l)) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_r)) T_r ... )))))
        "Join/Context/Op"
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r)))))
   
   (--> (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_l)) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_r)) T_r ... )))))
        "Join/Context/If"
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r)))))
   
   (--> (ς
         (in-hole F ((in-hole G (T ... (in-hole H (in-hole ACtx_l S_l)) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (in-hole ACtx_r S_r)) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_l)) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (in-hole (⊔/ACtx ACtx_l ACtx_r) S_r)) T_r ... )))))
        "Join/Context/App"
        (side-condition (not (term (≡/ACtx ACtx_l ACtx_r)))))
   
   
   
   |#
   
   #|
#lang racket
(match
  (list
   (bind 'F hole)
   (bind
    'G
    '((((λ f (λ x hole))
        (λ x
          (λ y
            (if (or (string? x)
                    (string? y))
              (string-append x y)
              (+ x y)))))
       @
       ι14
       (⊤ → ⊥))
      @
      ι10
      (Number? → Number?)))
   (bind
    'H
    '((((λ f (λ x hole))
        (λ x
          (λ y
            (if (or (string? x)
                    (string? y))
              (string-append x y)
              (+ x y)))))
       @
       ι14
       (⊤ → ⊥))
      @
      ι10
      (Number? → Number?)))
   (bind 'S_l '((f 1) x))
   (bind 'S_r '(-blame ♭))))
|#
   
   
   
   
   
   #|
   (--> (ς
         (in-hole F ((in-hole G S_l)
                     ∥
                     (in-hole H S_r))))
        (ς
         (in-hole F ((in-hole G (⊔/Term S_l S_r))
                     ∥
                     (in-hole H (⊔/Term S_r S_l)))))
        "Join/Term"
        ;(side-condition (not (term (≡/Term S_l S_r)))))
        (side-condition (term (≈ G H)))
        (side-condition 
         (or 
          (redex-match? λCon-Subset (blame ♭) (term S_l))
          (redex-match? λCon-Subset (blame ♭) (term S_r))
          )
         )
        )
|#
   #|   
   (--> (ς
         (in-hole F ((in-hole G (op T ... (in-hole H S_l) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H S_r) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (op T ... (in-hole H (⊔/Term S_l S_r)) T_l ... ))
                     ∥
                     (in-hole G (op T ... (in-hole H (⊔/Term S_r S_l)) T_r ... )))))
        "Join/Term/Op"
        ;(side-condition (not (term (≡/Term S_l S_r)))))
        (side-condition 
         (or 
          (redex-match? λCon-Join (blame ♭) (term (S_l)))
          (redex-match? λCon-Join (blame ♭) (term (S_r)))))
        )
   
   (--> (ς
         (in-hole F ((in-hole G (if T ... (in-hole H S_l) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H S_r) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (if T ... (in-hole H (⊔/Term S_l S_r)) T_l ... ))
                     ∥
                     (in-hole G (if T ... (in-hole H (⊔/Term S_r S_l)) T_r ... )))))
        "Join/Term/If"
        (side-condition 
         (or 
          (redex-match? λCon-Join (blame ♭) (term (S_l)))
          (redex-match? λCon-Join (blame ♭) (term (S_r)))))
        )
        ;(side-condition (not (term (≡/Term S_l S_r)))))
   
   (--> (ς
         (in-hole F ((in-hole G (T ... (in-hole H S_l) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H S_r) T_r ... )))))
        (ς
         (in-hole F ((in-hole G (T ... (in-hole H (⊔/Term S_l S_r)) T_l ... ))
                     ∥
                     (in-hole G (T ... (in-hole H (⊔/Term S_r S_l)) T_r ... )))))
        "Join/Term/App"
        (side-condition 
         (or 
          true
          (redex-match? λCon-Join (blame ♭) (term (S_l)))
          (redex-match? λCon-Join (blame ♭) (term (S_r)))))
        )
;          (term (≡/Term S_l S_r)) (term (≡/Term S_l S_r)))))
;        (side-condition (not (term (≡/Term S_l S_r)))))
   
   |#
   ))

; ((L M N) .... (M @ ♭ C) (V @ ι C) (blame ♭) (M @ ι C))
;  
;  ((L M N) K x (λ x M) (M N) (op M ...) (if L M N))
;  ;; Baseline Reduction Context
;  ;; --------------------------
;  ((F G H) hole (λ x F) (F M) (T F) (op T ... F M ...) (if T ... F M ...) (F @ b C));






















#|


(define-metafunction λCon-Subset
  ≈ : G H -> boolean
  
  [(≈ any any) #t]
  
  [(≈ (blame ♭) H) #t]
  [(≈ G (blame ♭)) #t]
  
  
  
  ;; what, if only left a contarct
  
  
  [(≈ (G @ ι_l C_l) H) (≈ G H)]
  [(≈ G (H @ ι_r C_r)) (≈ G H)]
  
  [(≈ (op T ... G M ...) (op T ... H N ...)) ,(and
                                               (term (≈ any_l0 any_r0))
                                               (term (≈ (any_l1 ...) (any_r1 ...))))]
  
  
  
  ;[(≈ (any_l @ ι C) (any_r @ ι C)) (≈ any_l any_r)]
  
  ;[(≈ (any_l @ ι_l C_l) (any_r @ ι_r C_r)) (≈ any_l any_r)]
  [(≈ (any_l0 any_l1 ...) (any_r0 any_r1 ...)) ,(and
                                                 (term (≈ any_l0 any_r0))
                                                 (term (≈ (any_l1 ...) (any_r1 ...))))]
  [(≈ any ... ) #f])

|#

#|




(define-metafunction λCon-Subset
  ≈ : any any -> boolean
  
  [(≈ any any) #t]
  
  [(≈ (blame ♭) any) #t]
  [(≈ any (blame ♭)) #t]
  
  ;; what, if only left a contarct
  
  [(≈ (any_l @ ι_l C_l) any_r) (≈ any_l any_r)]
  [(≈ any_l (any_r @ ι_r C_r)) (≈ any_l any_r)]
  
  ;[(≈ (any_l @ ι C) (any_r @ ι C)) (≈ any_l any_r)]
  
  ;[(≈ (any_l @ ι_l C_l) (any_r @ ι_r C_r)) (≈ any_l any_r)]
  [(≈ (any_l0 any_l1 ...) (any_r0 any_r1 ...)) ,(and
                                                 (term (≈ any_l0 any_r0))
                                                 (term (≈ (any_l1 ...) (any_r1 ...))))]
  [(≈ any ... ) #f])

|#
;  [(≈/M M M) #t])

#|
(define-metafunction λCon-Subset
  ≈/M : M M -> boolean
  
  [(≈/M M M) #t]
  
  
  [(≈/M M M) #t] 
  
 |# 

#|

(define-metafunction λCon-Subset
  ≡/F : F F -> boolean
  
  [(≡/F F F) #t] 
  
  [(≡/F (λ x G) (λ x H)) (≡/F G H)]
  [(≡/F (G M)   (H M))   (≡/F G H)]
  [(≡/F (T G)   (T H))   (≡/F G H)]
  
  [(≡/F (op T ... G M ...)   (op T ... H M ...))   (≡/F G H)]
  [(≡/F (if T ... G M ...)   (if T ... H M ...))   (≡/F G H)]
  
  [(≡/F (T G)   (T H))   (≡/F G H)]
  
  
  ;; false
  [(≡/F (λ x G) (λ y H)) (≡/F G H)]
  
  
  
  [(≡/F (ACtx_0 @ ι C) (ACtx_1 @ ι C)) (≡/ACtx ACtx_0 ACtx_1)] ;;
  ;; TO make this deterministiv, say that the upper contract must be different
  ;; same for terms
  [(≡/F hole hole) #t] 
  [(≡/F any ...) #f])

|#


;; Problem, term is not the deepest term

;(⊔/Term 
; ((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y))))) 
; ((λ f (λ x ((-blame ♭) @ ι11 ⊥))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x ;y))))))

;
;(((λ f (λ x ((f 1) x)))   
;(((λ f (λ x ((-blame ♭) @ ι11 ⊥))) 
;  
;  (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y))))) @ ι10 (Number? → Number?))
;  (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y))))) @ ι10 (Number? → Number?))

;(⊔/Term 
; ((λ f (λ x ((f 1) x))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x y))))) 
; ((λ f (λ x ((-blame ♭) @ ι11 ⊥))) (λ x (λ y (if (or (string? x) (string? y)) (string-append x y) (+ x ;y))))))


;; merge
(define-metafunction λCon-Subset
  ⊔/ACtx : ACtx ACtx -> ACtx
  ;  [(⊔/ACtx ACtx_l ACtx_r) (in-hole ACtx_l ACtx_r)])
  ;  [(⊔/ACtx hole ACtx)  ACtx]
  ;  [(⊔/ACtx ACtx hole)  ACtx]
  [(⊔/ACtx ACtx_l ACtx_r)  (in-hole ACtx_l (\\ ACtx_r ACtx_l))])
;;

(define-metafunction λCon-Subset
  ∈/ACtx : C ACtx -> boolean
  [(∈/ACtx C (ACtx @ ι C)) #t]
  [(∈/ACtx C (ACtx @ ι D)) (∈/ACtx C ACtx)]
  [(∈/ACtx C hole) #f])  

;; context set minus
(define-metafunction λCon-Subset
  \\ : ACtx ACtx -> ACtx
  [(\\ (ACtx_0 @ ι C) ACtx) (\\ ACtx_0 ACtx) (side-condition (term (∈/ACtx C ACtx)))]
  [(\\ (ACtx_0 @ ι C) ACtx) ((\\ ACtx_0 ACtx) @ ι C) (side-condition (not (term (∈/ACtx C ACtx))))]
  [(\\ hole hole) hole]
  [(\\ hole ACtx) hole])

(define-metafunction λCon-Subset
  ≡/ACtx : ACtx ACtx -> boolean
  [(≡/ACtx (ACtx_0 @ ι C) (ACtx_1 @ ι C)) (≡/ACtx ACtx_0 ACtx_1)] ;;
  ;; TO make this deterministiv, say that the upper contract must be different
  ;; same for terms
  [(≡/ACtx hole hole) #t] 
  [(≡/ACtx any ...) #f])

(define-metafunction λCon-Subset
  ≡/Term : T T -> boolean
  [(≡/Term T T) #t] 
  [(≡/Term any ...) #f])

;; TODO, maybe we need a one level equivalence of contetx


(define-metafunction λCon-Subset
  ⊔/Term : T T -> T
  ;[(⊔/Term (blame b) (blame b)) (blame ♭) (where (blame ♭) (blame-of b ς))]
  
  [(⊔/Term (blame ♭) T) T]
  [(⊔/Term T (blame ♭)) T]
  [(⊔/Term T T_x) T])



#|

(define-metafunction λCon-Baseline
  ≈ : T T -> boolean
  ;; equals terms
  [(≈ T T) #t]
  ;; equal contracts on differnt terms
  [(≈ (T_l @ ι C) (T_r @ ι C)) #t]
  ;; one side blame
  [(≈ (blame ♭) T) #f]
  [(≈ T (blame ♭)) #f]
  ;; differnt terms without contract
  [(≈ S_l S_r) #t]
  ;; othweise
  [(≈ any ...) #f])



(define-metafunction λCon-Baseline
  √ : ∥ T T -> T
  ;; intersection/ negative blame
  [(√ (∩∩ b) (-blame ♭) T) T]
  [(√ (∩∩ b) T (-blame ♭)) T]
  ;; intersection/ positive blame
  [(√ (∩∩ b) (+blame ♭) T) (+blame ♭)]
  [(√ (∩∩ b) T (+blame ♭)) (+blame ♭)]
  ;; union/ negative blame
  [(√ (∪∪ b) (-blame ♭) T) (-blame ♭)]
  [(√ (∪∪ b) T (-blame ♭)) (-blame ♭)]
  ;; union/ positive blame
  [(√ (∪∪ b) (+blame ♭) T) T]
  [(√ (∪∪ b) T (+blame ♭)) T]
  
  ;; XXX
  [(√ (∩∩ b) T S) T]
  [(√ (∪∪ b) T S) T]) ;; TODO

|#


;; TODO, use join with (∩∩ ♭)

#|
  (define-metafunction λCon-Baseline
    ⊔/x : T T -> T
    ;; intersection/ negative blame
    [(⊔/x ∩∩ (-blame ♭) T) T]
    [(⊔/x ∩∩ T (-blame ♭)) T]
    ;; intersection/ positive blame
    [(⊔ ∩∩ (+blame ♭) T) (+blame ♭)]
    [(⊔ ∩∩ T (+blame ♭)) (+blame ♭)]
    ;; union/ negative blame
    [(⊔ ∪∪ (-blame ♭) T) (-blame ♭)]
    [(⊔ ∪∪ T (-blame ♭)) (-blame ♭)]
    ;; union/ positive blame
    [(⊔ ∪∪ (+blame ♭) T) T]
    [(⊔ ∪∪ T (+blame ♭)) T])
 |# 








#|
    _     _      
 _ | |___(_)_ _  
| || / _ \ | ' \ 
 \__/\___/_|_||_|
                 
|#
;; TODO, change join function.
;; A context/subject blame may only be removed if it is a blame term from the splitted contract.
;; Thus, say ∥ = (∪∪ ♭).

#|

(define-metafunction λCon-Baseline
  join : ∥ M M -> M
  ;; intersection/ negative blame
  [(join ∩∩ (-blame ♭) T) T]
  [(join ∩∩ T (-blame ♭)) T]
  ;; intersection/ positive blame
  [(join ∩∩ (+blame ♭) T) (+blame ♭)]
  [(join ∩∩ T (+blame ♭)) (+blame ♭)]
  ;; union/ negative blame
  [(join ∪∪ (-blame ♭) T) (-blame ♭)]
  [(join ∪∪ T (-blame ♭)) (-blame ♭)]
  ;; union/ positive blame
  [(join ∪∪ (+blame ♭) T) T]
  [(join ∪∪ T (+blame ♭)) T])

|#

#|
 ___            _ _         _                         _ 
| _ \_ _ ___ __| (_)__ __ _| |_ ___ ___  __ _ _ _  __| |
|  _/ '_/ -_) _` | / _/ _` |  _/ -_|_-< / _` | ' \/ _` |
|_| |_| \___\__,_|_\__\__,_|\__\___/__/ \__,_|_||_\__,_|
                                                        
 ___             _   _             
| __|  _ _ _  __| |_(_)___ _ _  ___
| _| || | ' \/ _|  _| / _ \ ' \(_-<
|_| \_,_|_||_\__|\__|_\___/_||_/__/
                                   
|#

;; Join Reduction (λCon/Join-->)
;; -----------------------------
(define
  (λCon/Join~~> ς configuration)
  (if (redex-match? λCon-Baseline (ς T) configuration)
      (car (apply-reduction-relation Join-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))

;; Join Reduction (λCon/Join-->*)
;; ------------------------------
(define
  (λCon/Join~~>* configuration)
  (if (redex-match? λCon-Baseline (ς T) configuration)
      (car (apply-reduction-relation* Join-reduction (term ,configuration)))
      (error "Invalid λCon-term:" configuration)))