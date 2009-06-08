#lang scheme
(require redex "traverse-graph.ss")

(test-equal
 (traverse-graph 100
  (λ (node)
   (if (even? node) (list (quotient node 2))
    (list (add1 (* node 3)) (quotient (add1 (* node 3)) 2)))))
'(1 2 4 8 16 5 10 20 40 13 26 52 17 34 11 22 44 88 29 58 19 38 76 25 50 100))

(test-equal
 (traverse-graph 100
  (λ (node)
   (if (even? node)
    (build-list 2 (λ (i) (quotient node (+ i 2))))
     (list (add1 (* node 3)) (quotient (add1 (* node 3)) 2)))))
'(20 40 13 26 52 17 34 11 7 14 22 10 3 44 88 29
  58 6 0 1 2 4 12 19 38 76 5 8 16 25 33 50 100))

(test-equal
 (traverse-graph '(0 0)
  (λ (node)
   (let-values (((x y) (apply values node)))
    (filter (λ (x) x)
     (list
      (and (< x 3) (list (add1 x) y))
      (and (< y 2) (list x (add1 y))))))))
'((3 2) (3 1) (2 2) (1 2) (2 1) (3 0) (2 0) (0 2) (1 1) (0 1) (1 0) (0 0)))

(test-results)
