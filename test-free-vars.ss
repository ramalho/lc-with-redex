#lang scheme ; File test-free-vars.ss
(require "free-vars.ss" redex)
(printf "~a~n" "test-free-vars")
(test-equal (term (Free-vars (λ (x) (x x)))) (term ( )))
(test-equal (term (Free-vars (λ (x) (x y)))) (term (y)))
(test-equal (term (Free-vars (λ (x) ((x y) z)))) (term (z y)))
(test-equal (term (Free-vars ((λ (x) (x y)) (λ (y) (x y))))) (term (y x)))
(test-equal (term (Var-free-in? x (λ (y) z))) #f)
(test-equal (term (Var-free-in? x (λ (x) x))) #f)
(test-equal (term (Var-free-in? x (λ (y) x))) #t)
(test-results) ; Displays: All 7 tests passed.
