#lang scheme ; File normal-order-beta-reductor-version-2.ss
(require redex)
(require (only-in "curried-lc-grammar.ss" curried-lc-grammar))
(require (only-in "free-vars.ss" Free-vars))
(require (only-in "beta-reductor.ss" Subst))
(provide normal-order-β-reductor-version-2 βNf? curried-lc-language)
(provide Normal-form)
(printf "~a~n" "test-normal-order-beta-reductor-version-2")

(define-extended-language curried-lc-language curried-lc-grammar
 (‹nf› (λ (‹var›) ‹nf›) ‹non-abstr-nf›)
 (‹non-abstr-nf› ‹var› (‹non-abstr-nf› ‹nf›))
 (‹hole› hole
  (λ (‹var›) ‹hole›)
  (‹non-abstr-nf› ‹hole›)
  (side-condition (‹hole›_1 ‹term›) (not (term (Abstr? ‹hole›_1))))))

(define normal-order-β-reductor-version-2
 (reduction-relation curried-lc-language
  (--> (in-hole ‹hole› ((λ (‹var›) ‹term›_1) ‹term›_2))
       (in-hole ‹hole› (Subst (‹var› ‹term›_2) ‹term›_1)))))

(define-metafunction curried-lc-language Abstr? : any -> any
 ((Abstr? (λ (‹var›) any)) #t)
 ((Abstr? any) #f))

(define-metafunction curried-lc-language βNf? : any -> any
 ((βNf? ‹nf›) #t)
 ((βNf? any) #f))

(define-metafunction curried-lc-grammar Normal-form : ‹term› -> ‹term›
 ((Normal-form (side-condition ‹term› (term (βNf? ‹term›)))) ‹term›)
 ((Normal-form ‹term›)
  (Normal-form
  ,(car
    (apply-reduction-relation
     normal-order-β-reductor-version-2
     (term ‹term›))))))