#lang scheme ; file test-beta-normal-form.ss
(require redex "curry.ss")
(require (only-in "normal-order-beta-reductor-version-2.ss" Normal-form))
(provide test-beta-normal-form)
(define (test-beta-normal-form expr expected)
 (test-equal (term (Normal-form (Curry ,expr))) (term (Curry ,expected))))