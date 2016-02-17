#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Constraint Generation
;; =====================

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x (x @ ♭ Number?))) 1)))
 (term (((♭ ◃ ι) ·) ((λ x (+ x (x @ ι Number?))) 1))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x 1)) (x @ ♭ Number?))))
 (term (((♭ ◃ ι) ·) ((λ x (+ x 1)) (x @ ι Number?)))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x 1)) (((+ x 1) @ ♭ Number?) @ ♭1 Positive?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((λ x (+ x 1)) (((+ x 1) @ ι Number?) @ ι1 Positive?)))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x (x @ ♭ (Number? ∩ Odd?)))) 1)))
 (term (((♭ ◃ ι) ·) ((λ x (+ x (x @ ι (Number? ∩ Odd?)))) 1))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x (x @ ♭ (Number? ∪ Odd?)))) 1)))
 (term (((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)) ((λ x (+ x ((x @ ι1 Number?) @ ι2 Odd?))) 1))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x (x @ ♭ ((Number? → Number?) ∩ (Odd? → Odd?))))) 1)))
 (term (((♭ ◃ ι) ·) ((λ x (+ x (x @ ι ((Number? → Number?) ∩ (Odd? → Odd?))))) 1))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x (x @ ♭ ((Number? → Number?) ∪ (Odd? → Odd?))))) 1)))
 (term (((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)) ((λ x (+ x ((x @ ι1 (Number? → Number?)) @ ι2 (Odd? → Odd?)))) 1))))


;; Valid Contracts
;; ===============

(test-->>
 Pre-evaluation
 (term (· (((λ x (+ x 1)) @ ♭ (⊤ → ⊤)) 1)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) ((λ x (+ x 1)) 1))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x 1)) (1 @ ♭ ⊤))))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) ((λ x (+ x 1)) 1))))

;; Test: Pre-evaluation
;; ====================

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x (1 @ ♭ Number?))) 1)))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) ((λ x (+ x 1)) 1))))

(test-->>
 Pre-evaluation
 (term (· ((1 @ ♭ Number?) @ ♭1 Positive?)))
 (term (((ι1 ◃ (#t ∘ #t)) ((♭1 ◃ ι1) ((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)))) 1)))

(test-->>
 Pre-evaluation
 (term (· ((0 @ ♭ Number?) @ ♭1 Positive?)))
 (term (((♭1 ◃ ι1) ((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·))) (0 @ ι1 ⊥))))

(test-->> 
 Pre-evaluation
 (term (· ((1 @ ♭ Positive?) @ ♭1 Number?)))
 (term (((ι1 ◃ (#t ∘ #t)) ((♭1 ◃ ι1) ((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)))) 1)))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ x 1)) (1 @ ♭ Number?))))
 (term (((ι ◃ (#t ∘ #t)) ((♭ ◃ ι) ·)) ((λ x (+ x 1)) 1))))

(test-->>
 Pre-evaluation
 (term (· ((λ x (+ (x @ ♭ Number?) 1)) 1)))
 (term (((♭ ◃ ι) ·) ((λ x (+ (x @ ι Number?) 1)) 1))))

(test-results)