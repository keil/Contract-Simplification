#lang racket



 #|
  ;; Non-function terms
  (non-Lambda K x (+blame ♭) (-blame ♭) (non-Tq Ti) (non-Lambda Topt) (op Topt ...) (if Topt_0 Topt_1 Topt_2)
              (non-Lambda @ ι C))
  
  ;; Non-value Terms
  (non-Value (+blame ♭) (-blame ♭) (non-Tq Ti) (non-Lambda Topt) (op Topt ...) (if Topt_0 Topt_1 Topt_2)
             (non-Value @ ι C) )
  
  ;; Non-contracted Terms
  (non-Contract K x (+blame ♭) (-blame ♭) (λ x non-Contract) (non-Tq Ti) (non-Lambda Topt) (op Topt ...) (if Topt_0 Topt_1 Topt_2)
                )
  
  ;; With-Immediate
  (Ti non-Value (Ti @ ι I))
  
  ;; Non-Delayed
  (non-Tq non-Contract Ti)
  ;; With-Delayed
  (Tq Ti non-Contract (Tq @ ι Q))
  
  ;; Optimized Term
  (Topt Tq Ti)
  
  
  
  ;; Immediate Contracts 
  (Immediate
   (+blame ♭) (-blame ♭) 
   (Canonical Canonical) (Canonical_0 Canonical_1) (op Canonical ...) (if Canonical_0 Canonical_1 Canonical_2)
   ;; Immediate Contract
   (Immediate @ ι I)
   )
  
  ;; Delayed Contracts
  (Delayed
   K x (+blame ♭) (-blame ♭) 
   (λ x Canonical) (Canonical Canonical) (Canonical_0 Canonical_1) (op Canonical ...) (if Canonical_0 Canonical_1 Canonical_2)
   Immediate
   ;; Delayed Contract
   (Delayed @ ι Q)
   )
  (Canonical 
   K x (+blame ♭) (-blame ♭) 
   
   (λ x Canonical) (Canonical_0 Canonical_1) (op Canonical ...) (if Canonical_0 Canonical_1 Canonical_2)
   Delayed
   )
  |#
  



  
  #|
  
  ;; Terms without a contract
  (S
   K x (+blame ♭) (-blame ♭) 
   (λ x S) (S_0 S_1) (op T ...) (if T_0 T_1 T_2)
   
   
   
   (x TI)     
   (S TI)
   (x T) (K T) (TI T) ((blame ♭) T)
   
   
   (K T) (x T) ((blame ♭) T)  
   (S TQ)
   ((op T ...) T_1) ((if T_0 T_1 T_2) T)
   )
  
  (TI (S_0 S_1) (op T ...) (if T_0 T_1 T_2)
      (TI @ ι I)
      )
  
  (TQ 
   ;K x (λ x S) (S_0 S_1) (op T ...) (if T_0 T_1 T_2) 
   S TI
   (TQ @ ι Q) (TI @ ι Q)
   )
  
  ;; Full-optimized termes
  (T S TQ TI
     ;(x TI)
     ;
     ;(S TI)
     ;
     ;(x T) (K T) (TI T) ((blame ♭) T)
     ;(S_0 S_1) (op S ...) (if S_0 S_1 S_2)
     ;(+blame ♭) (-blame ♭)
     )
  
  
  
  
  
  
  
  |#
  

 ;; Contract-free terms (λJ terms)
  ;; ------------------------------
  ;((S T) K x (λ x T) (S T) (op T ...) (if S T_0 T_1))
  
  ;; Non-reducable terms (stucked assertions)
  ;; ----------------------------------------
  (B
   ;; Contract-free terms
   T
   ;; Imemdiate Contracts
   ;(x @ ι I); TODO
   ((B_0 B_1) @ ι I)
   ((op B ...) @ ι I)
   ((if B_0 B_1 B_2) @ ι I)
   ;; Delayed Contracts
   ; TODO
   
   
   
   ;(B @ ι Q); TODO eg.g X @ Q cannot be simpified, and (op T @ Q
   
   ;(x @ ι Q); TODO
   
   ;; Blame terms
   (blame ♭))
  
  ;; TODO
  (A B (A @ ι Q))