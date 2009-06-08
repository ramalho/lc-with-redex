#lang scheme	; File curry.ss
(require redex "uncurried-lc-grammar.ss")
(provide Curry)

(define-metafunction uncurried-lc-grammar Curry : ‹term› -> ‹term›
 ((Curry (λ () ‹term›)) (Curry ‹term›))
 ((Curry (λ (‹var›_0 ‹var›_1 ...) ‹term›))
  (λ (‹var›_0) (Curry (λ (‹var›_1 ...) ‹term›))))
 ((Curry (‹term›)) (Curry ‹term›))
 ((Curry (‹term›_0 ‹term›_1)) ((Curry ‹term›_0) (Curry ‹term›_1)))
 ((Curry (‹term›_0 ‹term›_1 ‹term›_2 ...))
  (Curry ((‹term›_0 ‹term›_1) ‹term›_2 ...)))
 ((Curry ‹var›) ‹var›))
