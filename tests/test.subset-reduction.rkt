#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Number?) @ ♭1 Number?)))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Number?) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 Positive?)))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) (Positive? ∩ Even?)) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Positive?) @ ♭1 (Positive? ∩ Odd?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Positive?) @ ι1 Odd?))))
 
(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Positive?) @ ♭1 (Positive? ∩ Odd?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Number?) @ ι1 (Positive? ∩ Odd?)))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 (Positive? ∩ Even?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) (Positive? ∩ Even?)) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 (Positive? ∪ Even?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) (Positive? ∩ Even?)) @ ι1 ⊤))))

;(test-->>
; Subset-reduction
; (term (· ((x @ ♭ (Positive? ∪ Even?)) @ ♭1 (Number? ∩ Even?))))
; (term (((ι ◃ (ι1 ∪ ι2)) ((♭ ◃ ι) ·)) (((x @ (ι1 ι2) Positive?) @ ι2 Even?) @ ♭1 (Number? ∩ Even?)))))
;; test fails because the uniuon gets unrolled
;; say., the pre evluator only untolls unions between differnt types



(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Number?) @ ♭1 Number?)))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Number?) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Positive?) @ ♭1 Number?)))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Positive?) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 Number?)))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) (Positive? ∩ Even?)) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Positive?) @ ♭1 (Number? ∩ Odd?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Positive?) @ ι1 Odd?))))
 
(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Number?) @ ♭1 (Positive? ∩ Odd?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) Number?) @ ι1 (Positive? ∩ Odd?)))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 (Number? ∩ Even?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) (Positive? ∩ Even?)) @ ι1 ⊤))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 (Number? ∪ Even?))))
 (term (((♭1 ◃ ι1) ((♭ ◃ ι) ·)) ((x @ (ι ι1) (Positive? ∩ Even?)) @ ι1 ⊤))))



(test-results)