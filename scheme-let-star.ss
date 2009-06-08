#lang scheme
(define-syntax let*
 (syntax-rules ()
  ((_ () . body)
   ((λ () . body)))
  ((_ ((var value) binding ...) . body)
   ((λ (var) (let* (binding ...) . body)) value))))

(let* ((a 1) (b (add1 a)) (c (+ a b))) (list a b c))