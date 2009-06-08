#lang scheme ; File test-pairs.ss
(require redex "pairs.ss" "lazy-evaluator.ss")
(printf "~a~n" "test-pairs")
(test-equal (ev (Car (Cons yes no))) 'yes)
(test-equal (ev (Cdr (Cons yes no))) 'no)
(test-results)
