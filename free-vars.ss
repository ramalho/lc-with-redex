#lang scheme	; File free-vars.ss
(require redex "curried-lc-grammar.ss")
(provide Free-vars Var-free-in?)

(define-metafunction curried-lc-grammar Free-vars : ‹term› -> (‹var› ...)
 ((Free-vars ‹var›) (‹var›))
 ((Free-vars (‹term›_0 ‹term›_1))
  (Union (Free-vars ‹term›_0) (Free-vars ‹term›_1)))
 ((Free-vars (λ (‹var›) ‹term›))
  (Remove-var ‹var› (Free-vars ‹term›))))

(define-metafunction curried-lc-grammar
 Union : (‹var› ...) (‹var› ...) -> (‹var› ...)
 ((Union (‹var›_0 ...) ()) (‹var›_0 ...))
 ((Union () (‹var›_1 ...)) (‹var›_1 ...))
 ((Union (‹var›_0 ‹var›_1 ...) (‹var›_2 ... ‹var›_0 ‹var›_3 ...))
  (‹var›_0 ,@(term (Union (‹var›_1 ...) (‹var›_2 ... ‹var›_3 ...)))))
 ((Union (‹var›_2 ... ‹var›_0 ‹var›_3 ...) (‹var›_0 ‹var›_1 ...))
  (‹var›_0 ,@(term (Union (‹var›_1 ...) (‹var›_2 ... ‹var›_3 ...)))))
 ((Union (‹var›_0 ‹var›_1 ...) (‹var›_2 ‹var›_3 ...))
  (‹var›_0 ‹var›_2 ,@(term (Union (‹var›_1 ...) (‹var›_3 ...))))))

(define-metafunction curried-lc-grammar
 Remove-var : ‹var› (‹var› ...) -> (‹var› ...)
 ((Remove-var ‹var›_0 (‹var›_1 ... ‹var›_0 ‹var›_2 ...))
  (‹var›_1 ... ‹var›_2 ...))
 ((Remove-var ‹var› (‹var›_0 ...)) (‹var›_0 ...)))

(define-metafunction curried-lc-grammar Var-free-in? : ‹var› ‹term› -> any
 ((Var-free-in? ‹var›_0 ‹var›_0) #t)
 ((Var-free-in? ‹var›_0 ‹var›_1) #f)
 ((Var-free-in? ‹var›_0 (λ (‹var›_0) ‹term›)) #f)
 ((Var-free-in? ‹var›_0 (λ (‹var›_1) ‹term›)) (Var-free-in? ‹var›_0 ‹term›))
 ((Var-free-in? ‹var› (‹term›_0 ‹term›_1))
 ,(or
   (term (Var-free-in? ‹var› ‹term›_0))
   (term (Var-free-in? ‹var› ‹term›_1)))))
