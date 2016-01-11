#lang racket
(require redex)

(require "lj.rkt")
(require "lcon.rkt")
(require "baseline.rkt")
(require "contracts.rkt")

#|
 _                   _         _         _          
| |_ ___ _ _ _ __   (_)_ _  __| |_  _ __(_)___ _ _  
|  _/ -_) '_| '  \  | | ' \/ _| | || (_-< / _ \ ' \ 
 \__\___|_| |_|_|_| |_|_||_\__|_|\_,_/__/_\___/_||_|
                                                    
|#

;; Term Inclusion
;; --------------
;; Each reduced term S is a valid λCon term M

(define 
  (λCon-term? N)
  (redex-match? λCon M N))

;(redex-check λCon-Baseline S (λCon-term? (term S)) #:attempts 100000)

#|
            _         _ _    _     
 _ _ ___ __| |_  _ __(_) |__| |___ 
| '_/ -_) _` | || / _| | '_ \ / -_)
|_| \___\__,_|\_,_\__|_|_.__/_\___|

|#

;; Lemma: reducible (progress)
;; ---------------------------
;; Each term M is either final (non-reducible)
;; or it exists a reduction step

(define
  (reducible? M)
  (= (length (apply-reduction-relation Baseline-reduction2 M)) 1))

;(redex-check λCon M (if (not (done? (term M))) (reducible? (term M)) #t) #:attempts 100000)

#|
                                   _   _          
 _ __ _ _ ___ ___ ___ _ ___ ____ _| |_(_)___ _ _  
| '_ \ '_/ -_|_-</ -_) '_\ V / _` |  _| / _ \ ' \ 
| .__/_| \___/__/\___|_|  \_/\__,_|\__|_\___/_||_|
|_|                                               

|#

;; Lemma: preservation
;; -------------------
;; After each reduction step M ~> N
;; M -> V and N -> V must reduce to the same value 
;; After each reduction sequence M ~> S
;; M -> V and S -> V must reduce to the same value 

(define
  (reduce M)
  (car (apply-reduction-relation* Baseline-reduction M)))

(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))

(define
  (compare M)
  (eq? (evaluate (reduce M)) (evaluate M)))

;(redex-check λCon M (compare (term M)) #:attempts 100000)

;; Note:
;; This Lemma first needs to check of both expressions are valid expressions

#|                       
 ____  _ __ __ ___ ______
(_-< || / _/ _/ -_|_-<_-<
/__/\_,_\__\__\___/__/__/
                         
|#

;; Lemma: success
;; --------------
;; Let M be a λCon term, S be the optimization (M ~> S) of M,
;; x (y) be the number of predicate checks when evaluating M (S)
;; it holds that x >= y

;; We may restrict the result to x > y to say that we always benefit 
;; from he reduction, e.g. we can also say that the number of predicate 
;; checks decrease with every reduction step M ~> N

;; Note, success cannot ne verified by counting the top-level assertions.
;; It requires to count e.g. predicate checks during the evaluation.

#|
 _    _                
| |__| |__ _ _ __  ___ 
| '_ \ / _` | '  \/ -_)
|_.__/_\__,_|_|_|_\___|
                       
|#

;; Lemma: blame
;; ------------
;; Let M be a λCon term, S be the optimization (M ~> S) of M.
;; S == blame(i) iff for every value V (M V) --> blame(i).

;; Lemma: blame
;; ------------
;; Let M be a λCon term, S be the optimization (M ~> S) of M,
;; S --> blame(i) iff M --> blame(i)