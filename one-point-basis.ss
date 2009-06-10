#lang scheme ; File one-point-basis
(require redex "curry.ss" "curried-lc-grammar.ss" "free-vars.ss")
(require "lazy-evaluator.ss")
(printf "~a~n" "one-point-basis")

(def-proc 'X
 (let ((K '(λ (x y) x)) (S '(λ (x y z) ((x z) (y z)))))
  (term (Curry (λ (x) (x ,K ,S ,K))))))

(define-language x-term (‹xterm› (variable-except λ) (‹xterm› ‹xterm›)))
(define-metafunction x-term check-xterm : ‹xterm› -> #t ((check-xterm ‹xterm›) #t))

(define-metafunction curried-lc-grammar Trafo : ‹term› -> ‹term›
 ((Trafo I) (Trafo ((S K) K)))
 ((Trafo K) ((X X) X))
 ((Trafo S) (X (X X)))
 ((Trafo X) X)
 ((Trafo ‹var›) ‹var›)
 ((Trafo (λ (‹var›_0) ‹var›_0)) (Trafo I))
 ((Trafo
   (side-condition
    (λ (‹var›_0) ‹term›_0)
    (not (term (Var-free-in? ‹var›_0 ‹term›_0)))))
  ((Trafo K) (Trafo ‹term›_0)))
 ((Trafo (λ (‹var›) (‹term›_0 ‹term›_1)))
  (((Trafo S) (Trafo (λ (‹var›) ‹term›_0))) (Trafo (λ (‹var›) ‹term›_1))))
 ((Trafo (λ (‹var›) ‹term›)) (Trafo (λ (‹var›) (Trafo ‹term›))))
 ((Trafo (‹term›_0 ‹term›_1)) ((Trafo ‹term›_0) (Trafo ‹term›_1))))

(define-syntax test
 (syntax-rules ()
  ((_ p q)
   (let* ((x (term (Trafo (Curry p)))))
    (printf "~s ->~n" 'p) (pretty-display x) (printf "-> ~s~n~n" 'q)
    (term (check-xterm ,x)) ; Checks Trafo to produce an ‹xterm›.
    (test-equal (ev-proc x) 'q)))))

(test ((λ (x) x) yes) yes)
(test ((λ (x y) x) yes no) yes)
(test ((λ (x y) y) yes no) no)
(test (X X a b c) b)
(test (S K X (S K K) b) b)
(test (S K K b) b)
(test (S K S b) b)
(test (K yes no) yes)
(test (K I yes no) no)
(test (K I I yes) yes)
(test (K I I I I yes) yes)
(test (S I I K a b c) b)
(test (S S S S S S S I I I yes) yes)
(test (S (S S) (S S) (S S) S S K yes) yes)
(test (X X X X X X X I I yes) yes)
(test (X (X X) (X X) (X X) X X I I yes) yes)

(test-results)
