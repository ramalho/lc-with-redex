#lang scheme ; File normal-order-beta-reductor-version-1.ss
(require redex "curried-lc-grammar.ss" "curry.ss")
(require "beta-reductor.ss")

(define (normal-order-β-reductor T)
 (let loop ((T T) (n (reduction-steps-cutoff)))
  (and (> n 0)
   (let*
    ((reducts (sort (apply-reduction-relation β-reductor T) term<?))
     (k (length reducts))
     (i (and (positive? k) (random k))))
    (printf "~s ~s ~s~n" k i T)
    (if i (loop (list-ref reducts (random (length reducts))) (sub1 n)) T)))))

(define (term<? x y) (string<? (format "~s" x) (format "~s" y)))
  
(define-metafunction curried-lc-grammar βNf? : ‹term› -> ‹bool›
 ((βNf? ‹var›) #t)
 ((βNf? (λ (‹var›) ‹term›)) (βNf? ‹term›))
 ((βNf? (‹term›_0 ‹term›_1))
 ,(and
   (not (term (Abstr? ‹term›_0)))
   (term (βNf? ‹term›_0))
   (term (βNf? ‹term›_1)))))

(define-syntax test
 (syntax-rules ()
  ((_ (x y) ...)
   (begin
    (begin
     (test-equal
      (normal-order-β-reductor (term (Curry x)))
      (and 'y (term (Curry y))))
     (newline)) ...))))

(random-seed 2)
(reduction-steps-cutoff 20)

(test
 (((((λ (x y) x) y) z) ((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z))
  (y (a (a (a (a z))))))
 (((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z) (a (a (a (a z)))))
 ((λ (x) (y x)) (λ (x) (y x)))
 (((λ (x) x) y) y)
 (((λ (x y z) (x y z)) a b c) (a b c))
 (((λ (z) ((a b) z)) c) (a b c))
 (((λ (x) x) ((λ (x) y) ((λ (x) (x x)) (λ (x) (x x))))) y)
 (((λ (x) (x x)) (λ (x) (x x))) #f))

(test-results)