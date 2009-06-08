#lang scheme ; File pair-numerals.ss
(require "pairs.ss" "booleans.ss" "lazy-evaluator.ss")

(def Zero (λ (x) x))
(def Zero? Car)
(def Add1 (Cons False))
(def Sub1 Cdr)
(def Y (λ (m) ((λ (f) (m (f f))) (λ (f) (m (f f))))))

(def +
 (Y (λ (+ x y) (If (Zero? x) y (If (Zero? y) x (Add1 (Add1 (+ (Sub1 x) (Sub1 y)))))))))

(def - (Y (λ (- x y) (If (Zero? x) Zero (If (Zero? y) x (- (Sub1 x) (Sub1 y)))))))

(def *
 (Y
  (λ (* x y)
   (If (Zero? x) Zero (If (Zero? y) Zero (Sub1 (+ (+ (* (Sub1 x) (Sub1 y)) x) y)))))))

(def =
 (Y (λ (= x y) (If (Zero? x) (Zero? y) (If (Zero? y) False (= (Sub1 x) (Sub1 y)))))))

(def > (Y (λ (> x y) (If (Zero? x) False (If (Zero? y) True (> (Sub1 x) (Sub1 y)))))))
(def < (λ (x y) (> y x)))
(def >= (λ (x y) (Not (< x y))))
(def <= (λ (x y) (Not (> x y))))
(def Quotient (Y (λ (Quotient x y) (If (< x y) Zero (Add1 (Quotient (- x y) y))))))
(def Modulo (λ (x y) (- x (* (Quotient x y) y))))
(def Divisor? (λ (x y) (Zero? (Modulo x y))))

(def Even?+Odd?
 (Y
  (λ (Even?+Odd?)
   ((λ (Even? Odd?)
     (Cons
      (λ (n) (If (Zero? n) True (Odd? (Sub1 n))))
      (λ (n) (If (Zero? n) False (Even? (Sub1 n))))))
    (λ (n) (Car Even?+Odd? n))
    (λ (n) (Cdr Even?+Odd? n))))))

(def Even? (Car Even?+Odd?))
(def Odd? (Cdr Even?+Odd?))

(define (number->pair-numeral n)
 (if (zero? n) (ev Zero)
  ((ev (Cons False)) (number->pair-numeral (sub1 n)))))

(define (pair-numeral->number numeral)
 (if (eq? ((ev (λ (numeral) (If (Zero? numeral) yes no))) numeral) 'yes) 0
  (add1 (pair-numeral->number ((ev Sub1) numeral)))))

(provide number->pair-numeral pair-numeral->number)
