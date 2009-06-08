#lang scheme ; File tools.ss
(require redex)
(provide test-true test-false)
(define (test-true  expr) (test-equal expr #t))
(define (test-false expr) (test-equal expr #f))
