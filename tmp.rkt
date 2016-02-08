#lang racket
(require redex)
(require rackunit)

(require "lcon.rkt")



(redex-match? 
 λCon M
 (term ((+ 1 2) @ ♭ (flat %Positive %Even))))




(test-results)