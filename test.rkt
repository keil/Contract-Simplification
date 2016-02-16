#lang racket

#|
 _____       _      
|_   _|__ __| |_ ___
  | |/ -_|_-<  _(_-<
  |_|\___/__/\__/__/
                    
|#

;; Test: λJ
;; --------

(require "tests/test.lj.rkt")

;; Test: λCon
;; ----------

(require "tests/test.lcon.rkt")

;; Baseline/Pre-evaluation
;; -----------------------

(require "tests/test.pre-evaluation.rkt")

;; Sub-Contract Relation
;; ---------------------

(require "tests/test.subset.rkt")

;; Contract Substraction
;; ---------------------

(require "tests/test.setminus.rkt")

;; Baseline/Subset-reduction
;; -------------------------

(require "tests/test.subset-reduction.rkt")