#lang scheme ; File normal-order-test.ss
(require redex "tracer.ss")
(provide normal-order-test)

(define-syntax normal-order-test
 (syntax-rules ()
  ((_ grammar reductor βNf?)
   (begin
    (tracer reductor
     (((((λ (x y) x) y) z) ((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z))
      (y (a (a (a (a z))))))
     (((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z) (a (a (a (a z)))))
     ((λ (x) (y x)) (λ (x) (y x)))
     (((λ (x) x) y) y)
     (((λ (x y z) (x y z)) a b c) (a b c))
     (((λ (z) ((a b) z)) c) (a b c))
     (((λ (x) (x x)) (λ (x) (x x))) #f))
    (redex-check grammar ‹term›
     (let*
      ((nf? (term (βNf? ‹term›)))
       (redices
        (apply-reduction-relation
         reductor
         (term ‹term›))))
      (cond
       ((and (pair? redices) (pair? (cdr redices)))
        (error 'test "more than one left most redex for term ~s"
         (term ‹term›)))
       ((and nf? (pair? redices))
        (error 'test "(βNf? ~s)->#t but reductor found redex ~s."
         (term ‹term›) (car redices)))
       ((and (not nf?) (null? redices))
        (error 'test "(βNf? ~s)->#f but reductor found no redex ~s."
         (term ‹term›)))))
     #:attempts 10000 #:retries 100)))))