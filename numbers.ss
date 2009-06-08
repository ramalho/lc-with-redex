#lang scheme ; file numbers.ss
(require redex "curry.ss" "booleans.ss" "pairs.ss")
(provide Zero Zero? Add1 Sub1)

(define Zero  '(λ (x) x))
(define Zero? Car)
(define Add1  `(,Cons ,False))
(define Sub1  Cdr)

(test `(,Zero? ,Zero yes no) 'yes)
(test `(,Zero? (,Add1 ,Zero) yes no) 'no)
(test `(,Zero? (,Sub1 (,Add1 (,Add1 ,Zero))) yes no) 'no)
(test `(,Zero? (,Sub1 (,Sub1 (,Add1 (,Add1 ,Zero)))) yes no) 'yes)

(test-results) ; Displays: All 4 tests passed.
