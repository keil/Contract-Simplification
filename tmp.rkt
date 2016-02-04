#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")


(define
  (evaluate M)
  (car (apply-reduction-relation* λCon-reduction M)))





;(redex-match? λCon ♭ (term ♭1))
;(variable-not-in (term (+ ♭1 ♭1)) (term ♭))
;(variable-not-in (term (+ ι y)) (term ι))
;(fresh (term (+ ι y)))

(define λCon-value?
  (redex-match? λCon V))




(define-metafunction λCon
  Σ : P -> (M ...)
  [(Σ ⊤) ((λ x #t))]
  [(Σ ⊥) ((λ x #f))]
  ;[(Σ (⊤ / M)) (M)]
  [(Σ (P / M)) (⊕ (Σ P) (M))]
  [(Σ predefined) (Σ (lookup/ predefined))]
  )


(define-metafunction λCon
  ⊕ : (M ...) (M ...) -> (M ...)
  [(⊕ (M ...) ()) (M ...)]
  [(⊕ () (M ...)) (M ...)]
  [(⊕ (M_0 ... M_n M_i ...) (M_n M_m ...)) (⊕ (M_0 ... M_n M_i ...) (M_m ...))]
  [(⊕ (M_0 ...) (M_n M_m ...)) (⊕ (M_0 ... M_n) (M_m ...))])




      (in-hole E (V @ (eval ,(with-handlers 
                                    ([(λ x #t) (lambda (exn) (term #f))])
                                  (evaluate (term (M V))))))))

(side-condition (not (false? (term W))))