#lang racket
(require redex)

(require "../baseline.rkt")
(require "../examples/examples.rkt")

(provide (all-defined-out))

#|
                   _ _           _     _        _   
 _  _ _ _  _ _ ___| | |  ____  _| |__ (_)___ __| |_ 
| || | ' \| '_/ _ \ | | (_-< || | '_ \| / -_) _|  _|
 \_,_|_||_|_| \___/_|_| /__/\_,_|_.__// \___\__|\__|
                                    |__/            
|#


(test-->
 Baseline-reduction
 example:unroll/subject/0
 (term ((λ f (f 1)) (λ x 1))))

;(traces Baseline-reduction2 example:unroll/subject/0)



(test-->
 Baseline-reduction
 example:unroll/subject/1
 (term ((λ x (x 1)) (λ x 1))))

;(traces Baseline-reduction2 example:unroll/subject/1)









(test-->
 Baseline-reduction
 example:unroll/subject/2
 (term ((λ x (x 1)) (λ x 1))))

;(traces Baseline-reduction example:unroll/subject/2)




#|
                   _ _              _           _   
 _  _ _ _  _ _ ___| | |  __ ___ _ _| |_ _____ _| |_ 
| || | ' \| '_/ _ \ | | / _/ _ \ ' \  _/ -_) \ /  _|
 \_,_|_||_|_| \___/_|_| \__\___/_||_\__\___/_\_\\__|
                                                    
|#