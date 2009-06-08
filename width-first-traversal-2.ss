#lang scheme ; file width-first-traversal.ss
#|
Width first traversal of a graph starting from a given node.
Procedure (traverse S G) -> void 
For arbitrary node type T:
S : T             ; starting node.
G : T -> (T ...)  ; graph.

Procedure G represents a graph. It is called with a node T for its argument and is expected to return the list of all nodes having an inbound branch from node T. The returned list may contain duplicates, but they will be ignored by procedure traverse. Nodes are compared to each other with procedure equal?. G is called exactly once for every node that can be reached from node S, node S included.

Legend:
H : hash((T . any) ...) done, hash of nodes already processed.
F : (T ...) frontier, list of nodes whose outbound branches have not yet been followed.
|#

(define (traverse S G)
 (define H (make-hash))
 (define (put! T) (hash-set! H T #f))
 (define (new? T) (hash-ref H T (λ () #t)))
 (let loop ((F (list S)))
  (when (pair? F)
   (for-each put! F)
   (loop (remove-duplicates (filter new? (apply append (map G F))))))))

; TESTS

(require (only-in redex test-equal test-results))

(define (test-Collatz n)
 (test-equal
  (let ((r '()))
   (traverse n
    (λ (node) ; reversed Collatz sequence (linear graph)
     (set! r (cons node r))
     (cond
      ((= node 1) '())
      ((even? node) (list (quotient node 2)))
      (else (list (add1 (* node 3)))))))
   (reverse r))
  (let Collatz ((n n))
   (if (= n 1) '(1)
    (cons n (Collatz (if (even? n) (/ n 2) (add1 (* 3 n)))))))))

(for ((i (in-range 1 100))) (test-Collatz i))

(define (test-finite-rectangular-grid nx ny)
 (define r '())
 (define (G node) ; Rectangular grid with branches down and to the right
  (when (member node r)
   (error 'test-finite-rectangular-grid  "duplicate node"))
  (set! r (cons node r))
  (let ((x (car node)) (y (cadr node)))
   ((λ (new) (append new new new))
    (filter (λ (p) p)
     (list
      (and (< x nx) (list (add1 x) y))
      (and (< y ny) (list x (add1 y))))))))
 (test-equal
  (begin (traverse '(0 0) G) (sort-number-pairs r))
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
  (when (hash-has-key? H T) (error 'test-fully-connected  "duplicate node in: ~s" H))
  (hash-set! H T #t)
  nodes)
 (traverse 0 G)
 (test-equal (sort (hash-map H (λ (x y) x)) <) nodes))

(for ((n (in-range 1 100))) (test-fully-connected n))

(test-results)
