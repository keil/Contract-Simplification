#lang racket
(require redex)
(require rackunit)

(require "../baseline.rkt")

(test-->>
 Subset-reduction
 (term (· ((1 @ ♭ Number?) @ ♭1 Number?)))
 (term (· 1)))


(traces
 Subset-reduction
 (term (· ((x @ ♭ Number?) @ ♭1 Number?))))

(test-results)