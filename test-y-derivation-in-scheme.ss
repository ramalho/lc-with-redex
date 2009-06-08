#lang scheme
(require (only-in redex test-equal test-results))
(printf "~a~n" "test-y-derivation-in-scheme")
(letrec
 ((printer
   (λ (datum (port 'ignored))
    (unless (void? datum)
     (set! n (add1 n))
     (set! results (cons datum results)))))
  (n 0)
  (results '()))
 (parameterize ((current-print printer))
  (eval '(require "y-derivation-in-scheme.ss") (make-base-namespace))
  (printf "~s~n" results)
  (test-equal results (build-list n (λ (x) 24)))))

(test-results)