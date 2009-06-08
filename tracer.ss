#lang scheme
; Syntax (tracer (Term Expected) ...)
; Calls procedure tarce of redex for each Term and checks the result.
; Expected being #f means that no normal form is expected.
; Expected being a term means that a normal form is expected to be found
; and to be α-congruent with Expected. Both Term and Expected may be in
; uncurried form. They will be curried. If no nf is found, no check is made.

(require redex "alpha-congruence.ss" "curry.ss")
(provide tracer)

(define-syntax (tracer stx)
 (syntax-case stx ()
  ((_ reductor (Term Expected) ...)
   (with-syntax ; Reverse terms in order to get the guis in right order.
    (((Term ...) (reverse (syntax->list #'(Term ...))))
     ((Expected ...) (reverse (syntax->list #'(Expected ...)))))
  #'(begin
     (traces reductor (term (Curry Term))
      #:pred (λ (T) (check T 'Term 'Expected reductor))) ...)))))

(define (check T Term Expected reductor)
 (let ((nf? (null? (apply-reduction-relation reductor T))))
  (when nf?
   (when (not Expected)
    (error 'check "no nf expected, for term ~s, found nf ~s" Term T))
     (when
      (not (term (α-congruent? ,T ,(term (Curry ,Expected)))))
       (error 'check "expected nf ~s for term ~s, found ~s" Expected Term T)))
  (not nf?))) ; highlight if normal form (pink)
