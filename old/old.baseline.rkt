#lang racket


   
   ;; Deconstruct/ Reconstruct Contract
   ;; ---------------------------------
   
;   (--> (in-hole H ((λ x (in-hole A (x M))) @ (Q → D))) ;; ? all contracts? ;; every context
;        (in-hole H ((λ x (in-hole A ((x @ Q) M))) @ (⊤ → D)))
;        "Deconstruct/Domain"
;        (side-condition (not (canonical? (term (Q → D)))))
        ;; side condition: contract not in canonical form
;   )
   
;   (--> (in-hole H ((λ x V) @ (C → D))) ;; ? all contracts?
;        (in-hole H ((λ x (V @ D)) @ (C → ⊤))) ;; ? all contracts?
;        "Deconstruct/Range"
        ;; cicle
;        (side-condition (not (canonical? (term (C → D)))))
        ;; side condition: contract not in canonical form
;   )
   
  ; (--> (in-hole H ((λ x V) @ (C → I))) ;; ? all contracts?
  ;      (in-hole H ((λ x (V @ I)) @ (C → ⊤))) ;; ? all contracts?
  ;      "Deconstruct/Range2"
  ;      (side-condition (not (canonical? (term (C → I)))))
        ;; cicle
        ;; side condition: contract not in canonical form
  ; )