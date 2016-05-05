#lang racket
   (--> (
         (in-hole F ((in-hole H (T @ ι C)) || (in-hole H S))))
        (ς
         (in-hole F ((in-hole H (T @ ι C)) || (in-hole H (S @ ι C)))))
        "Join/LeftContract"
        
        (side-condition (and (canonical? (term (in-hole H (T @ ι C))))
                             (canonical? (term (in-hole H S)))))
        )
   
   (--> (ς
         (in-hole F ((in-hole H S) || (in-hole H (T @ ι C)))))
        (ς
         (in-hole F ((in-hole H (S @ ι C)) || (in-hole H (T @ ι C)))))
        "Join/RightContract"
        
        (side-condition (and (canonical? (term (in-hole H S)))
                             (canonical? (term (in-hole H (T @ ι C))))))
        ) 
   
   (--> (ς
         (in-hole F ((in-hole H (T_1 @ ι_1 C)) || (in-hole H (T_2 @ ι_2 D)))))
        (ς
         (in-hole F ((in-hole H ((T_1 @ ι_1 C) @ ι_2 D)) || (in-hole H ((T_2 @ ι_1 C) @ ι_2 D)))))
        "Join/LeftRightContract"
        
        (side-condition (and (canonical? (term (in-hole H (T_1 @ ι_1 C))))
                             (canonical? (term (in-hole H (T_2 @ ι_2 D))))
                             (not (eq? (term C) (term D)))
                             ))
        )




   (--> (ς
         (in-hole F ((in-hole G (T @ ι_1 C)) || (in-hole H (T @ ι_2 D)))))
        (ς
         (in-hole F ((in-hole G ((T @ ι_1 C) @ ι_2 D)) || (in-hole H ((T @ ι_1 C) @ ι_2 D)))))
        "Join/LeftRightContract"
        
        (side-condition (and (canonical? (term (T @ ι_1 C)))
                             (canonical? (term (T @ ι_2 D)))
                             (term (comparable? G H))))

        )  