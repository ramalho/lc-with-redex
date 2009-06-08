#lang scheme ; File let-start-in-terms-of-lambda
(define-syntax let*
 (syntax-rules ()
  ((let* () . body) ((λ () . body)))
  ((let* ((var expr) binding ...) . body)
   ((λ (var) (let* (binding ...) . body)) expr))))

(let* ((a 1) (b 2) (c (+ a b)) (a (+ b c)) (c (+ a b c))) (list a b c)) ; --> (5 2 10)
 