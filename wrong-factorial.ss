#lang scheme
(((λ (m) ((λ (f) (m (f f))) (λ (f) (m (f f)))))
  (λ (!) 'this-is-never-evaluated)) 'this-is-evaluated-but-the-value-is-never-used)