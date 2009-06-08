#lang scheme
(require redex)

(define (number->Church-numeral n)
 (define (f^n f n) (if (zero? n) (λ (x) x) (λ (x) ((f^n f (sub1 n)) (f x)))))
 (λ (f) (λ (x) ((f^n f n) x))))

(define (Church-numeral->number Ch-n) ((Ch-n add1) 0))

(define (test Church-numeral number)
 (test-equal (Church-numeral->number Church-numeral) number))

(let
 ((Church-add1  (λ(n)(λ(f)(λ(x)(f((n f)x))))))
  (Church-sub1  (λ(n)(λ(f)(λ(x)(((n(λ(g)(λ(h)(h(g f)))))(λ(u)x))(λ(u)u))))))
  (Church-plus  (λ(m)(λ(n)(λ(f)(λ(x)((m f)((n f)x)))))))
  (Church-mult  (λ(m)(λ(n)(λ(f)(n(m f))))))
  (Church-expt  (λ(m)(λ(n)(n m))))
  (Church-zero? (λ(n)((n(λ(x)#f))#t))))
 (let*-values
  (((Church-minus) (λ(m)(λ(n)((n Church-sub1)m))))
   ((first-ten-numbers) (build-list 10 (λ (n) n)))
   ((first-ten-Church-numerals) (map number->Church-numeral first-ten-numbers))
   ((C0 C1 C2 C3 C4 C5 C6 C7 C8 C9) (apply values first-ten-Church-numerals)))
  (for-each test first-ten-Church-numerals first-ten-numbers)
  (test ((Church-plus  C5) C7)  12)
  (test ((Church-mult  C5) C7)  35)
  (test ((Church-minus C5) C5)   0)
  (test ((Church-minus C7) C5)   2)
  (test ((Church-expt  C7) C3) 343)
  (test-equal (Church-zero? ((Church-minus C5) C5)) #t)
  (test-equal (Church-zero? ((Church-minus C5) C4)) #f)))

(test-results)
