#lang scheme ; File test-pair-numerals.ss
(require redex "pair-numerals.ss" "lazy-evaluator.ss" redex)
(printf "~a~n" "test-pair-numerals")

(for ((n (in-range 0 25)))
 (test-equal (pair-numeral->number (number->pair-numeral n)) n))

(for ((m (in-range 1 11)) #:when #t (n (in-range 1 11)))
 (for-each
  (λ (function check-function)
   (test-equal
    (pair-numeral->number
     ((function (number->pair-numeral m)) (number->pair-numeral n)))
    (check-function m n)))
  (list (ev +) (ev -) (ev *) (ev Modulo) (ev Quotient))
  (list + (λ (m n) (max 0 (- m n))) * modulo quotient)))

(for ((n (in-range 0 10)))
 (test-equal ((((ev Even?) (number->pair-numeral n)) #t) #f) (even? n))
 (test-equal ((((ev Odd?) (number->pair-numeral n)) #t) #f) (odd? n)))

(test-results)
               
               
