#lang scheme
#|
Width first traversal of a graph starting from a given staring node.

Procedure (traverse-graph G S) -> (T ...)
For arbitrary node type T:
G : T -> (T ...)
S : T

Procedure G must given a node T return the list of all nodes T has outbound branches to. The returned list may contain multiple occurences of the same node. The second and follwing occurrence of the same node will be ignored. Nodes are compared to each other with procedure equal?.

Procedure traverse-graph returns a list of all nodes that can be reached starting from the starting node S. The returned list contains no duplicates. The list is sorted in decreasing distance from the starting node, assuming each branch has the same positove length. 

R : (T ...) list of nodes found so far.
H : (T ...) list of nodes whose outbound branches have not yet been followed.
N : (T ...) list of new nodes found while following branches of the nodes of H.
B : (T ...) list of nodes with inbound branches from element of H.
K : T       element of B
|#

(define (traverse-graph S G)
 (let ((R (list S)))
  (let loop ((R R) (H R))
   (if (null? H) R
    (let H-loop ((R R) (H H) (N '()))
     (if (null? H) (loop R N)
      (let B-loop ((R R) (B (G (car H))) (N N))
       (if (null? B) (H-loop R (cdr H) N)
        (let ((K (car B)) (B (cdr B)))
         (if (member K R) (B-loop R B N)
          (B-loop (cons K R) B (cons K N))))))))))))

(provide traverse-graph)

