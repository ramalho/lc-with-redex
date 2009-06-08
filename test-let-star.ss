#lang scheme
(require redex "let-star.ss")

(define namespace (make-base-namespace))
(define (eval* x) (eval x namespace))
(define plus (eval* (λ (x) (λ (y) (λ (z) (+ x y z))))))

(test-equal
 (eval*
 `(let ((plus ,plus) (a 1))
  ,(term
    (Let* ((p a) (q (add1 p)) (r (add1 q))) (plus p q r)))))
 6)

(test-results)