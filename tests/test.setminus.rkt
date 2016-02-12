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

;; Contract Substraction (\\)
;; ==========================

(check-eq?
 (term (\\ Number? Number?))
 (term ⊤))

(check-eq?
 (term (\\ Positive? Number?))
 (term Positive?))

(check-eq?
 (term (\\ Natural? Number?))
 (term Natural?))

(check-eq?
 (term (\\ Number? Positive?))
 (term ⊤))

(check-eq?
 (term (\\ Number? Natural?))
 (term ⊤))

(check-eq?
 (term (\\ Number? Natural?))
 (term ⊤))

(check-equal?
 (term (\\ (Positive? ∩ Even?) Number?))
 (term (Positive? ∩ Even?)))

(check-equal?
 (term (\\ Number? (Positive? ∩ Even?)))
 (term ⊤))

(check-equal?
 (term (\\ (Positive? ∩ Even?) (Number? ∩ Even?)))
 (term Positive?))

(check-equal?
 (term (\\ (Number? ∩ Even?) (Positive? ∩ Even?)))
 (term ⊤))

(check-equal?
 (term (\\ (Positive? ∪ Even?) (Number? ∩ Even?)))
 (term ⊤))

(check-equal?
 (term (\\ (Number? ∪ Even?) (Positive? ∩ Even?)))
 (term ⊤))

(check-equal?
 (term (\\ (Positive? ∩ Even?) (Number? ∪ Even?)))
 (term (Positive? ∩ Even?)))

(check-equal?
 (term (\\ (Number? ∩ Even?) (Positive? ∪ Even?)))
 (term Even?))