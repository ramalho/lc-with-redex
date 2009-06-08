#lang scheme
(require redex)
(printf "~a~n" "applicative-order-y-combinator")
(define (! n) (if (zero? n) 1 (* n (! (sub1 n))))) 

(define (test1 n)
 (test-equal
  (((λ (m) ((λ (f) (m (f f))) (λ (f) (m (λ (x) ((f f) x))))))
    (λ (!) (λ (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
   n)
  (! n)))

(define (test2 n)
 (test-equal
  (((λ (m) ((λ (f) (f f)) (λ (f) (m (λ (x) ((f f) x))))))
    (λ (!) (λ (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
   n)
  (! n)))

(define (test3 n)
 (test-equal
  (((λ (m) ((λ (f) (m (λ (x) ((f f) x)))) (λ (f) (m (λ (x) ((f f) x))))))
    (λ (!) (λ (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
   n)
  (! n)))

(for ((n (in-range 0 11))) (test1 n) (test2 n) (test3 n))
(test-results)

