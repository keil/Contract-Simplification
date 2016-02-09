#lang racket
  
  ;; Predicate (refinement)
  ;; ----------------------
  (P ⊤ (P / M) predefined)
  
  (predefined %Number %Complex %Real %Rational %Integer %String %Boolean
              %Exact %Inexact %Zero  
              %Positive %Negative %Even %Odd %Natural  
              %UInteger %UEven %UOdd)
  
  