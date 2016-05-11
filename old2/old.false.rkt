   
   ;; Lower (down)
   ;; ------------
   ;; Rule [Lower] creates a new function contarct from the 
   ;; contract of the function's body.
   
   (--> (ς
         (in-hole F (λ x (T @ ι C))))
        (ς
         (in-hole F ((λ x T) @ ι (⊤ → C))))
        "Lower")
   
   
   ;; Switch Order
   ;; ------------
   ;; Rule [Switch] changes the order of contracts such that imemdiate contracts
   ;; can be checked right awar.
   
   (--> (ς
         (in-hole F ((T @ ι_0 I) @ ι_1 Q)))
        (ς
         (in-hole F ((T @ ι_1 Q) @ ι_0 I)))
        "Switch")
   
   ;; Valid Contracts
   ;; ---------------
   ;; Removes (termn True) contracts.
   
   (--> (ς
         (in-hole F (T @ ι True)))
        (ς
         ;        (((ι ◃ (τ #t)) ς)
         (in-hole F T))
        "Recude/True")
   
   ;   (--> (ς
   ;         (in-hole F (T @ ι False)))
   ;        (((ι ◃ (τ #f)) ς)
   ;         (in-hole F T))
   ;        "Recude/False")
   
   ;   (--> (ς
   ;         (in-hole F (λ x (in-hole F0 (T @ ι False)))))
   ;        (ς
   ;         (in-hole F ((λ x (in-hole F0 T)) @ ι (⊤ → ⊥)))) ;; TODO special false element for blame 
   ;        "Recude/False")
   
