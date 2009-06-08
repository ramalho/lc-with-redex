#lang scheme ; File alpha-congruence.ss
(require redex "curried-lc-grammar.ss")
(provide α-congruent?)

(define-metafunction curried-lc-grammar α-congruent? : ‹term› ‹term› -> ‹bool›
 ((α-congruent? ‹term›_1 ‹term›_2) (α-aux? (‹term›_1 ( )) (‹term›_2 ( )))))

(define-metafunction curried-lc-grammar
 α-aux? : (‹term› ‹env›) (‹term› ‹env›) -> ‹bool›
 ((α-aux? (‹var›_0 ‹env›_0) (‹var›_1 ‹env›_1))
 ,(let
   ((e0 (assq (term ‹var›_0) (term ‹env›_0)))
    (e1 (assq (term ‹var›_1) (term ‹env›_1))))
   (or (and e0 e1 (= (cadr e0) (cadr e1)))
    (and (not e0) (not e1) (eq? (term ‹var›_0) (term ‹var›_1))))))
 ((α-aux? ((λ (‹var›_0) ‹term›_0) ‹env›_0) ((λ (‹var›_1) ‹term›_1) ‹env›_1))
 ,(term-let ((g (new-scope)))
   (term
    (α-aux?
     (‹term›_0 ((‹var›_0 g) ,@(term ‹env›_0)))
     (‹term›_1 ((‹var›_1 g) ,@(term ‹env›_1)))))))
 ((α-aux? ((‹term›_0 ‹term›_1) ‹env›_0) ((‹term›_2 ‹term›_3) ‹env›_2))
 ,(and
   (term (α-aux? (‹term›_0 ‹env›_0) (‹term›_2 ‹env›_2)))
   (term (α-aux? (‹term›_1 ‹env›_0) (‹term›_3 ‹env›_2)))))
 ((α-aux? any_1 any_2) #f))

(define new-scope 
; Use semaphore because the guis of traces may run in concurrent threads.
 (let ((semaphore (make-semaphore 1)) (scope 0))
  (λ ( )
   (begin0 (begin (semaphore-wait semaphore) (set! scope (add1 scope)) scope)
    (semaphore-post semaphore)))))
