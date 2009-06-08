#lang scheme
(require "lazy-evaluator.ss" "pair-numerals.ss" redex)
(printf "~a~n" "primes")

(def Naturals
 ((Y
   (λ (make-naturals n)
    (Cons n (make-naturals (Add1 n)))))
  Zero))

(def filter
 (Y
  (λ (filter pred list)
   (If (pred (Car list))
    (Cons (Car list) (filter pred (Cdr list)))
    (filter pred (Cdr list))))))

(def Primes
 ((Y
   (λ (make-primes number-list)
    (Cons (Car number-list)
     (make-primes
      (filter (λ (n) (Not (Divisor? n (Car number-list)))) (Cdr number-list))))))
  (Cdr (Cdr Naturals))))

(define (list-numbers numbers n)
 (if (zero? n) '()
  (cons (pair-numeral->number ((ev Car) numbers))
   (list-numbers ((ev Cdr) numbers) (sub1 n)))))

(test-equal (list-numbers (ev Naturals) 20)
'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

(define (primes n)
 (let loop ((primes '()) (n 2) (k n))
  (cond
   ((zero? k) (reverse primes))
   ((ormap (λ (p) (zero? (modulo n p))) primes) (loop primes (add1 n) k))
   (else (loop (cons n primes) (add1 n) (sub1 k))))))

(test-equal (list-numbers (ev Primes) 25) (primes 25))

(test-results)


        

