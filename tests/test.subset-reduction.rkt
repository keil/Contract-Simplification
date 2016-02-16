#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")

(test-->>
 Subset-reduction
 (term (· ((x@ ♭ Number?) @ ♭1 Number?)))
 (term (((♭ ◃ ι) ·) 1)))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Positive?) @ ♭1 Number?)))
 (term (((♭ ◃ ι) ·) ((1 @ ι ⊤) @ ♭1 Number?))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ (Positive? ∩ Even?)) @ ♭1 Number?)))
 (term (((♭ ◃ ι) ·) ((1 @ ι ⊤) @ ♭1 Number?))))

(test-->>
 Subset-reduction
 (term (· ((x @ ♭ Positive?) @ ♭1 (Number? ∩ Odd?))))
 (term (((♭ ◃ ι) ·) ((1 @ ι ⊤) @ ♭1 Number?))))
 
;(traces
; Subset-reduction
; (term (· ((x @ ♭ Number?) @ ♭1 Number?))))

(test-results)