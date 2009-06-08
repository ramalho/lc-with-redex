#lang scheme ; File curried-lc-grammar.ss
;(require "define-language-with-metapredicates.ss")
(include "define-language-with-metapredicates.ss")
(provide curried-lc-grammar Term? Abstr? Appl? Var? Varlist? Env? Binding?)

(define-language-with-metapredicates curried-lc-grammar
 (‹term›    ‹var› ‹abstr› ‹appl› #:pred Term?   )
 (‹abstr›   (λ (‹var›) ‹term›)   #:pred Abstr?  )
 (‹appl›    (‹term› ‹term›)      #:pred Appl?   )
 (‹var›     (variable-except λ)  #:pred Var?    )
 (‹varlist› (‹var› ...)          #:pred Varlist?)
 (‹env›     (‹binding› ...)      #:pred Env?    )
 (‹binding› (‹var› number)       #:pred Binding?)
 (‹bool›    #f #t                               )
 (‹subterm› (λ (‹var›) ‹subterm›) (‹subterm› ‹term›) (‹term› ‹subterm›) hole))

