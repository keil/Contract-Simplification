#lang racket
(require redex)
(require rackunit)

(require "../lcon.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test λCon/ Syntax
;; =================

(check-eq?
 (redex-match? λCon M (term (1 @ ♭ Any?)))
 #t)

(check-eq?
 (redex-match? λCon M (term ((+ 1 2) @ ♭ Any?)))
 #t)

(check-eq?
 (redex-match? λCon M (term (1 @ ♭ (flat (λ x (+ 1 1))))))
 #t)

(check-eq?
 (redex-match? λCon M (term ((+ 1 2) @ ♭ (flat (λ x (+ 1 1))))))
 #t)

(check-eq?
 (redex-match? λCon M (term ((λ x (+ x 1)) @ ♭ (Any? → Any?))))
 #t)

(check-eq?
 (redex-match? λCon M (term (((λ x (+ x 1)) @ ♭ (Any? → Any?)) 1)))
 #t)

(check-eq?
 (redex-match? λCon M (term ((λ x 1) 1)))
 #t)

(check-eq?
 (redex-match? λCon M (term ((λ x 1) #t)))
 #t)

;; Test λCon/ Reduction
;; ====================

(test-->> 
 λCon-reduction 
 (term (· ((+ 1 2) @ ♭ Any?)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 3)))

(test-->> 
 λCon-reduction 
 (term (· ((+ 1 2) @ ♭ Any?)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 3)))

(test-->> 
 λCon-reduction 
 (term (· ((+ 1 2) @ ♭ Boolean?)))
 (term (((ι ◃ (#t ∘ #f)) ((♭ ◃ ι) ·)) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x (+ x 1)) @ ♭ (Number? → Number?)) 1)))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) 2)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x (+ x 1)) @ ♭ (Positive? → Positive?)) 0)))
 (term (((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x (- x 1)) @ ♭ (Positive? → Positive?)) 1)))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· ((((λ x (λ y (+ x y))) @ ♭ (Positive? → (Positive? → Positive?))) 1) 1)))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))) 2)))

(test-->> 
 λCon-reduction 
 (term (· ((λ x (x 1)) ((λ x (+ x 1)) @ ♭ (Positive? → Positive?)))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) 2)))

(test-->> 
 λCon-reduction 
 (term (· ((((λ y (λ x ((y x) 1))) @ ♭ ((Positive? → (Positive? → Positive?)) → (Positive? → Positive?))) (λ x (λ y (+ x y)))) 1)))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι8 ◃ (#t ∘ #t)) ((ι7 ◃ (#t ∘ #t)) ((ι6 ◃ (ι7 → ι8)) ((ι5 ◃ (#t ∘ #t)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))))))) 2)))


;; Test λCon/ Blame
;; ================

(test-->> 
 λCon-reduction 
 (term (· (1 @ ♭ Number?)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 1)))

(test-->> 
 λCon-reduction 
 (term (· (#t @ ♭ Number?)))
 (term (((ι ◃ (#t ∘ #f)) ((♭ ◃ ι) ·)) (+blame ♭))))

(test-->>
 λCon-reduction
 (term (· ((+ 1 2) @ ♭ (Positive? ∩ Even?))))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))

(test-->>
 λCon-reduction
 (term (· ((+ 1 2) @ ♭ (Positive? ∩ Odd?))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))) 3)))


(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (Number? → Number?)) 1)))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (Number? → Number?)) #t)))
 (term (((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x #t) @ ♭ (Number? → Number?)) 1)))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (x ↦ (Λ x Number?))) 1)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (x ↦ (Λ x (flat (λ x (= x 1)))))) 1)))
 (term(((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 2) @ ♭ (x ↦ (Λ x (flat (λ x (= x 1)))))) 1)))
 (term (((ι ◃ (#t ∘ #f)) ((♭ ◃ ι) ·)) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (y ↦ (Λ y (flat (λ x (= x y)))))) 1)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 2) @ ♭ (x ↦ (Λ x (flat (λ y (= x y)))))) 1)))
 (term (((ι ◃ (#t ∘ #f)) ((♭ ◃ ι) ·)) (+blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· (1 @ ♭ (Number? ∩ Boolean?))))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (#t @ ♭ (Number? ∩ Boolean?))))
 (term (((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (1 @ ♭ (Number? ∩ Positive?))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))) 1)))



(test-->> 
 λCon-reduction 
 (term (· (1 @ ♭ (Number? ∪ Boolean?))))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)))) 1)))

(test-->> 
 λCon-reduction 
 (term (· (#t @ ♭ (Number? ∪ Boolean?))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)))) #t)))

(test-->> 
 λCon-reduction 
 (term (· ("1" @ ♭ (Number? ∪ Boolean?))))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ ((Number? → Number?) ∩ (Boolean? → Boolean?))) 1)))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι6 ◃ (#t ∘ #t)) ((ι5 ◃ (#t ∘ #t)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #f)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))))))) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x #t) @ ♭ ((Number? → Number?) ∩ (Boolean? → Boolean?))) 1)))
 (term (((ι6 ◃ (#t ∘ #f)) ((ι5 ◃ (#t ∘ #t)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #f)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·))))))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ ((Number? → Number?) ∩ (Boolean? → Boolean?))) "1")))
 (term (((ι5 ◃ (#t ∘ #f)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #f)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 0) @ ♭ ((Number? → Number?) ∩ (Positive? → Positive?))) 1)))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι6 ◃ (#t ∘ #t)) ((ι5 ◃ (#t ∘ #t)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))))))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 0) @ ♭ ((Number? → Number?) ∩ (Positive? → Positive?))) 0)))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι6 ◃ (#t ∘ #t)) ((ι5 ◃ (#t ∘ #t)) ((ι1 ◃ (ι5 → ι6)) ((ι3 ◃ (#t ∘ #f)) ((ι2 ◃ (ι3 → ι4)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·)))))))) 0)))



(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (Number? ∩ (Number? → Number?))) 1)))
 (term (((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∩ ι2)) ((♭ ◃ ι) ·))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (Number? ∪ (Number? → Number?))) 1)))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)))))) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ x 1) @ ♭ (Number? ∪ (Number? → Number?))) #t)))
 (term (((ι3 ◃ (#t ∘ #f)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·))))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ x #t) @ ♭ (Number? ∪ (Number? → Number?))) 1)))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)))))) (+blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· (((λ f (f 1)) @ ♭ ((Number? → Number?) → Number?)) (λ x 1))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι1 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ f (f #t)) @ ♭ ((Number? → Number?) → Number?)) (λ x 1))))
 (term (((ι3 ◃ (#t ∘ #f)) ((ι1 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· (((λ f (f 1)) @ ♭ ((Number? → Number?) → Number?)) (λ x #t))))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι3 ◃ (#t ∘ #t)) ((ι1 ◃ (ι3 → ι4)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (-blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· ((((λ f (λ x 1)) @ ♭ (Number? → (Number? → Number?))) 1) 1)))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))) 1)))

(test-->> 
 λCon-reduction 
 (term (· ((((λ f (λ x #t)) @ ♭ (Number? → (Number? → Number?))) 1) 1)))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι3 ◃ (#t ∘ #t)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))) (+blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· ((((λ f (λ x 1)) @ ♭ (Number? → (Number? → Number?))) 1) #t)))
 (term (((ι3 ◃ (#t ∘ #f)) ((ι2 ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (-blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· (((λ f (f 1)) @ ♭ ((Number? → Number?) → Number?)) ((λ x 1) @ ♭1 (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #t)) ((ι5 ◃ (#t ∘ #t)) ((ι7 ◃ (#t ∘ #t)) ((ι6 ◃ (#t ∘ #t)) ((ι1 ◃ (ι6 → ι7)) ((ι4 ◃ (#t ∘ #t)) ((ι2 ◃ (ι4 → ι5)) ((ι ◃ (ι2 → ι3)) ((♭1 ◃ ι1) ((♭ ◃ ι) ·)))))))))) 1)))

(test-->> 
 λCon-reduction 
 (term (· (((λ f (f 1)) @ ♭ ((Number? → Number?) → Number?)) ((λ x #t) @ ♭1 (Number? → Number?)))))
 (term(((ι7 ◃ (#t ∘ #f)) ((ι6 ◃ (#t ∘ #t)) ((ι1 ◃ (ι6 → ι7)) ((ι4 ◃ (#t ∘ #t)) ((ι2 ◃ (ι4 → ι5)) ((ι ◃ (ι2 → ι3)) ((♭1 ◃ ι1) ((♭ ◃ ι) ·)))))))) (+blame ♭1))))

(test-->> 
 λCon-reduction 
 (term (· (((λ f (f #t)) @ ♭ ((Number? → Number?) → Number?)) ((λ x 1) @ ♭1 (Number? → Number?)))))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι2 ◃ (ι4 → ι5)) ((ι ◃ (ι2 → ι3)) ((♭1 ◃ ι1) ((♭ ◃ ι) ·))))) (+blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· ((λ f (+ (f 1) (f 1))) ((λ x 1) @ ♭ (Number? → Number?)))))
 (term (((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι ◃ (ι3 → ι4)) ((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))))) 2)))

(test-->> 
 λCon-reduction 
 (term (· ((λ f (+ (f 1) (f #t))) ((λ x 1) @ ♭ (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #f)) ((ι ◃ (ι3 → ι4)) ((ι2 ◃ (#t ∘ #t)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· ((λ f (+ (f #t) (f 1))) ((λ x 1) @ ♭ (Number? → Number?)))))
 (term (((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· ((λ f (+ (f 1) (f 1))) ((λ x #t) @ ♭ (Number? → Number?)))))
 (term (((ι2 ◃ (#t ∘ #f)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))) (+blame ♭))))



(test-->> 
 λCon-reduction 
 (term (· ((λ f (((λ g (λ f (g 1))) f) f)) ((λ x (if (= x 1) (f 0) 0)) @ ♭ (Number? → Number?)))))
 (term (((ι2 ◃ (#t ∘ #t)) ((ι4 ◃ (#t ∘ #t)) ((ι3 ◃ (#t ∘ #t)) ((ι ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))))) 0)))

(test-->> 
 λCon-reduction 
 (term (· ((λ f (((λ g (λ f (g #t))) f) f)) ((λ x (if (= x 1) (f 0) 0)) @ ♭ (Number? → Number?)))))
 (term (((ι1 ◃ (#t ∘ #f)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· ((λ f (((λ g (λ f (g 1))) f) f)) ((λ x (if (= x 1) (f #t) 0)) @ ♭ (Number? → Number?)))))
 (term (((ι3 ◃ (#t ∘ #f)) ((ι ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·))))) (-blame ♭))))

(test-->> 
 λCon-reduction 
 (term (· ((λ f (((λ g (λ f (g 1))) f) f)) ((λ x (if (= x 1) (f 0) #t)) @ ♭ (Number? → Number?)))))
 (term (((ι4 ◃ (#t ∘ #f)) ((ι3 ◃ (#t ∘ #t)) ((ι ◃ (ι3 → ι4)) ((ι1 ◃ (#t ∘ #t)) ((ι ◃ (ι1 → ι2)) ((♭ ◃ ι) ·)))))) (+blame ♭))))



(test-results)