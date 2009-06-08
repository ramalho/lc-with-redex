#lang scheme ; File test-normal-order-beta-reductor-version-1.ss
(require (only-in "curried-lc-grammar.ss" curried-lc-grammar))
(require "normal-order-beta-reductor-version-1.ss")
(require "normal-order-test.ss")
(printf "~a~n" "test-normal-order-beta-reductor-version-1")
(normal-order-test curried-lc-grammar normal-order-β-reductor-version-1 βNf?)

