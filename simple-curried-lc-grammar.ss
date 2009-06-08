#lang scheme ; File simple-curried-lc-grammar.ss
(require redex)

(define-language curried-lc-grammar
 (‹term›                      ; A lambda term is
  ‹var›                       ; a variable reference,
  (λ (‹var›) ‹term›)          ; an abstraction or
  (‹term› ‹term›))            ; an application.
 (‹var› (variable-except λ))) ; Exclude λ as valid variable.
