#lang scheme ; File test-alpha-congruence.ss
(require redex "alpha-congruence.ss")
(printf "~a~n" "test-alpha-congruence")

(define-syntax tester ; Does concurrent tests.
 (syntax-rules ()
  ((tester ‹term› expected)
   (void
    (set! nr-of-tests (add1 nr-of-tests))
    (thread
     (λ ()
      (test-equal (term ‹term›) expected)
      (semaphore-post sema)))))))

(define nr-of-tests 0)
(define sema (make-semaphore 0))

(tester
 (α-congruent?
  ((λ (x) (λ (y) (λ (z) ((x y) z)))) a)
  ((λ (p) (λ (q) (λ (r) ((p q) r)))) a))
 #t)

(tester
 (α-congruent?
  ((λ (x) (λ (y) (λ (z) ((x y) z)))) a)
  ((λ (x) (λ (y) (λ (z) ((x y) z)))) b))
 #f) ; Because free variable a ≠ free variable b.

(tester
 (α-congruent?
  (λ (x) (λ (y) (λ (z) ((z z) z))))
  (λ (x) (λ (x) (λ (x) ((x x) x)))))
 #t) ; Because x has different bindings.

(tester
 (α-congruent?
  (λ (x) (λ (y) (λ (z) ((z z) z))))
  (λ (x) (λ (z) (λ (y) ((z z) z)))))
 #f) ; Because x and z have different bindings.

(let loop ((nr-of-tests nr-of-tests))
 (if (zero? nr-of-tests) (test-results)
  (begin (semaphore-wait sema)
   (loop (sub1 nr-of-tests))))) ; Displays: All 4 tests passed.
