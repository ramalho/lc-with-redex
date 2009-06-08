#lang scheme ; File uncurried-lc-grammar.ss
(require redex "curried-lc-grammar.ss")
(provide uncurried-lc-grammar)

(define-extended-language uncurried-lc-grammar curried-lc-grammar
 (‹term› ‹var› (λ (‹var› ...) ‹term›) (‹term› ‹term› ...)))

