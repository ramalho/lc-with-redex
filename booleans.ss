#lang scheme ; file booleans.ss
(require "lazy-evaluator.ss")
(def True  (λ (x y) x))
(def False (λ (x y) y))
(def If    (λ (x y z) (x y z)))
(def And   (λ (x y) (If x y False)))
(def Or    (λ (x y) (If x x y)))
(def Not   (λ (x) (If x False True)))


