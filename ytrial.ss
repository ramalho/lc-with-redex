#lang scheme
(define Y4
 ((λ (x) (x x x x x x x x x x x x x x x x x x x x x x x x x x))
  (λ (a b c d e f g h i j k l m n o p q s t u v w x y z)
   (λ (r)
    (λ (X) ((r ((t h i s i s a f i x e d p o i n t c o m b i n a t o) r)) X))))))

((Y4 (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))) 4)
((Y4 (λ (length) (λ (lst) (if (null? lst) 0 (add1 (length (cdr lst))))))) '(a b c d))
