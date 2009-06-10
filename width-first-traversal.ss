#lang scheme ; file width-first-traversal.ss
#|
Width first traversal of a graph starting from a given node.
Procedure (traverse-graph S G) -> void 
For arbitrary node type T:
S : T             ; starting node.
G : T -> (T ...)  ; graph.

Procedure G represents a graph. It is called with a node T for its argument and is assumed to return the list of all nodes having an inbound branch from node T. The returned list may contain duplicates, but they will be ignored by procedure traverse-graph. Nodes are compared to each other with procedure equal?. G is called exactly once for every node that can be reached from node S, node S included.

Let V be the set of all nodes reachable from S. If V is finite, then procedure traversal halts after having processed all elements of V. If V is infinite, then procedure traverse-graph never returns. If T is an arbitrary element of V, then procedure traverse-graph guarantees that node T will be found and G will once be called with node T, even in case V is infinite. If V is infinite G may be designed such as to put a limit on the number of nodes and to escape to a continuation when the limit is exceeded.

Legend:
H : hash((T . #f) ...) done, hash of nodes already processed.
F : (T ...) frontier, list of nodes whose outbound branches have not yet been traced.
|#

(define (traverse-graph S G)
 (define H (make-hash))
 (define (put! T) (hash-set! H T #f))
 (define (new? T) (hash-ref H T (λ () #t)))
 (define (filtered-G T) (filter new? (G T)))
 (let next-frontier ((F (list S)))
  (when (pair? F) (for-each put! F)
   (next-frontier (remove-duplicates (apply append (map filtered-G F)))))))

; TESTS

(require (only-in redex test-equal test-results))

(define (Collatz-next n) (if (even? n) (/ n 2) (add1 (* 3 n))))
(define (Collatz-length n) (if (= n 1) 1 (add1 (Collatz-length (Collatz-next n)))))

(define (test-Collatz n)
 (test-equal
  (let ((r 0))
   (traverse-graph n
    (λ (node) ; reversed Collatz sequence (linear graph)
     (set! r (add1 r))
     (if (= node 1) '() (list (Collatz-next node)))))
   r)
  (Collatz-length n)))

(for ((i (in-range 1 100))) (test-Collatz i))

(define (test-finite-rectangular-grid nx ny)
 (define R '())
 (define (G T) ; Rectangular grid with branches down and to the right
  (when (member T R)
   (error 'test-finite-rectangular-grid  "duplicate node"))
  (set! R (cons T R))
  (let ((x (car T)) (y (cadr T)))
   ((λ (new) (append new new new))
    (filter (λ (p) p)
     (list
      (and (< x nx) (list (add1 x) y))
      (and (< y ny) (list x (add1 y))))))))
 (test-equal
  (begin (traverse-graph '(0 0) G) (sort-number-pairs R))
  (for*/list ((x (in-range 0 (add1 nx))) (y (in-range 0 (add1 ny)))) (list x y))))

(define (sort-number-pairs x)
 (sort x
  (λ (x y)
   (or (< (car x) (car y))
    (and (= (car x) (car y)) (< (cadr x) (cadr y)))))))

(for* ((x (in-range 10 30)) (y (in-range 20 40))) (test-finite-rectangular-grid x y))

(define (test-fully-connected n)
 (define nodes (build-list n (λ (k) k)))
 (define H (make-hash))
 (define (G T)
  (when (hash-has-key? H T) (error 'test-fully-connected "duplicate node ~s" T))
  (hash-set! H T #t)
  nodes)
 (traverse-graph 0 G)
 (test-equal (sort (hash-map H (λ (x y) x)) <) nodes))

(for ((n (in-range 1 100))) (test-fully-connected n))

(define (test-infinite-graph limit)
 (let ((n limit))
  (test-equal
   (let/ec ec
    (define R '())
    (define (G T)
     (if (zero? n) (ec (length R))
      (begin
       (set! n (sub1 n))
       (set! R (cons T R))
       (for/list ((T (in-range (add1 T) (+ T 10)))) T))))
    (traverse-graph 0 G))
   limit)))

(for ((limit (in-range 0 100))) (test-infinite-graph limit))

(test-results)
(collect-garbage)