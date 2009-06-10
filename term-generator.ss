#lang scheme ; File term-generator
(require redex)
(printf "~a~n" "term-generator")

(define-language x-lang
 (‹term› X (‹term› ‹term›))
 (‹hole› (‹hole› ‹term›) (‹term› ‹hole›) hole))

(define extend
 (reduction-relation x-lang
  (--> X (X X))
  (--> (in-hole ‹hole› X) (in-hole ‹hole› (X X)))))

(define (list-terms n) ; Lists all terms with n occurrences of X.
 (case n
  ((0) '( ))  ((1) '(X))
  (else (remove-duplicates (apply append (map apply-extend (list-terms (sub1 n))))))))

(define-metafunction x-lang Term<? : ‹term› ‹term› -> any
 ((Term<? X X) #f)
 ((Term<? X ‹term›) #t)
 ((Term<? ‹term› X) #f)
 ((Term<? (‹term›_0 ‹term›_1) (‹term›_0 ‹term›_2)) (Term<? ‹term›_1 ‹term›_2))
 ((Term<? (‹term›_0 ‹term›_1) (‹term›_2 ‹term›_3)) (Term<? ‹term›_0 ‹term›_2)))

(define (Catalan n) (quotient (! (* 2 n)) (* (! n) (! (add1 n)))))  ; (2n)!/(n!(n+1)!)
; These are Catalan numbers after the mathematician Eugène Charles Catalan.
; Catalan numbers solve many counting problems. Here it is used to count the number of fully
; curried terms with n+1 occurrences of X, n>0.

(define (sort-terms x) (sort x term<?))
(define (term<? x y) (term (Term<? ,x ,y)))
(define (apply-extend x) (apply-reduction-relation extend x))
(define (! n) (if (zero? n) 1 (* n (! (sub1 n)))))
(define print-terms #f)

(for ((n (in-range 1 10)))
 (let* ((r (sort (list-terms n) term<?)) (c (length r)) (C (Catalan (sub1 n))))
  (printf "nr of X: ~s, nr of terms: ~s~n" n c)
  (when print-terms (for-each (λ (r) (printf "~s~n" r)) r) (newline))
  (test-equal c C)))

(test-results)
