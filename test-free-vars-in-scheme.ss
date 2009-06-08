#lang scheme ; File test-free-vars-in-scheme.ss
(require "free-vars-in-scheme.ss" redex)
(printf "~a~n" "test-free-vars-in-scheme")
(define b 1)
(test-equal b 1)
(test-equal a a)
(test-equal ((λ (x) x) y) y)
(test-equal ((λ (x) x) b) 1)
(test-results)