#lang scheme ; file beta-reductor.ss
(require redex "curried-lc-grammar.ss" "free-vars.ss")
(provide β-reductor Subst βnf?)

(define β-reductor
 (reduction-relation curried-lc-grammar
  (--> (in-hole ‹subterm› ((λ (‹var›) ‹term›_0) ‹term›_1))
       (in-hole ‹subterm› (Subst (‹var› ‹term›_1) ‹term›_0)) "β")))

(define-metafunction curried-lc-grammar βnf? : ‹term› -> ‹bool›
 ((βnf? ‹term›) ,(null? (apply-reduction-relation β-reductor (term ‹term›)))))

; Metafunction Subst does the substitution for β-contraction

(define-metafunction curried-lc-grammar Subst : (‹var› ‹term›) ‹term› -> ‹term›
 ; Matching var: replace ‹var›_0 with ‹term›_0.
 ((Subst (‹var›_0 ‹term›_0) ‹var›_0) ‹term›_0)
 ; Non matching var: no replacement.
 ((Subst (‹var›_0 ‹term›_0) ‹var›_1) ‹var›_1)
 ; Subst in abstr that shadows the var: no substitution in body.
 ((Subst (‹var›_0 ‹term›_0) (λ (‹var›_0) ‹term›_1)) (λ (‹var›_0) ‹term›_1))
 ; Subst in abstr that does not shadow the var: β contraction (= substitution)
 ((Subst (‹var›_0 ‹term›_0) (λ (‹var›_1) ‹term›_1))
 ,(term-let ((Free (term (Free-vars ‹term›_0))))
   (if (member (term ‹var›_1) (term Free))
    ; α-conversion required.
    (term-let 
     ((‹new-var›
       (variable-not-in
        (term (‹var›_0 Free (Free-vars ‹term›_1)))
        (term ‹var›_1))))
     (term
      (λ (‹new-var›)
       (Subst (‹var›_0 ‹term›_0) (Subst (‹var›_1 ‹new-var›) ‹term›_1)))))
    ; α-conversion not required.
    (term (λ (‹var›_1) (Subst (‹var›_0 ‹term›_0) ‹term›_1))))))
 ; Application: recur on operator and operand.
 ((Subst (‹var›_0 ‹term›_0) (‹term›_1 ‹term›_2))
  ((Subst (‹var›_0 ‹term›_0) ‹term›_1) (Subst (‹var›_0 ‹term›_0) ‹term›_2))))
