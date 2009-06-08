#lang scheme ; File normal-order-beta-reductor-version-1.ss
(require redex "curried-lc-grammar.ss")
(require (only-in "beta-reductor.ss" Subst))
(provide normal-order-β-reductor-version-1 βNf? β-redex?)

(define normal-order-β-reductor-version-1
 (reduction-relation curried-lc-grammar
  (--> ‹term› (Contract-once ‹term›)
   (side-condition (not (term (βNf? ‹term›)))))))

(define-metafunction curried-lc-grammar βNf? : ‹term› -> ‹bool›
 ((βNf? ‹var›) #t)
 ((βNf? (λ (‹var›) ‹term›)) (βNf? ‹term›))
 ((βNf? (‹term›_0 ‹term›_1))
 ,(and
   (not (term (Abstr? ‹term›_0)))
   (term (βNf? ‹term›_0))
   (term (βNf? ‹term›_1)))))

(define-metafunction curried-lc-grammar β-redex? : ‹term› -> ‹bool›
 ((β-redex? ((λ (‹var›) ‹term›_0) ‹term›_1)) #t)
 ((β-redex? any) #f))
 
(define-metafunction curried-lc-grammar Contract-once : ‹term› -> ‹term›
 ((Contract-once (side-condition ‹term› (term (β-redex? ‹term›))))
  (Contract-β-redex ‹term›))
 ((Contract-once (λ (‹var›) ‹term›)) (λ (‹var›) (Contract-once ‹term›)))
 ((Contract-once
   ((side-condition ‹term›_0 (not (term (βNf? ‹term›_0)))) ‹term›_1))
  ((Contract-once ‹term›_0) ‹term›_1))
 ((Contract-once (‹term›_0 ‹term›_1)) (‹term›_0 (Contract-once ‹term›_1))))

(define-metafunction curried-lc-grammar Contract-β-redex : ‹term› -> ‹term›
 ((Contract-β-redex ((λ (‹var›_0) ‹term›_0) ‹term›_1))
  (Subst (‹var›_0 ‹term›_1) ‹term›_0)))
