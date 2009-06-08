#lang scheme ; file pairs.ss
(require "booleans.ss" "lazy-evaluator.ss")
(def Cons (λ (x y z) (If z x y)))
(def Car  (λ (x) (x True)))
(def Cdr  (λ (x) (x False)))

