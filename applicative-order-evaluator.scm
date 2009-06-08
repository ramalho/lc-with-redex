#lang scheme
(require (only-in redex test-equal test-results))

; Simple normal order evaluator for lambda terms.

; T ::= V                     ; lambda term: variable reference
; T ::= (λ (V ...) T)         ; lambda term: abstraction
; T ::= (T T ...)             ; lambda term: application
; V ::= identifier, but not λ ; variable
; E ::= (B ...)               ; environment
; B ::= ((V any) ...)         ; binding

(define (eval T) (eval-aux (curry T) '()))

; Procedure curry:
; (curry V)         --> V
; (λ () T)          --> T
; (T)               --> T
; (λ (V_1 V ...) T) --> (λ (V_1) (curry (λ (V ...) T)))
; (T_1 T_2 T ...)   --> (curry ((T_1 T_2) T ...))

(define (curry T)
 (define (var? x) (and (symbol? x) (not (eq? x 'λ))))
 (define (appl? x) (and (pair? x) (list? x)))
 (define (abstr? x)
  (and (list? x) (= (length x) 3) (eq? (car x) 'λ) (var? (caadr x))))
 (define (curry-abstr T)
  (let ((body (curry (caddr T))))
   (let curry-abstr ((vars (cadr T)))
    (if (null? vars) body
    `(λ (,(car vars)) ,(curry-abstr (cdr vars)))))))
 (define (curry-appl T)
  (let ((T (map curry T)))
   (let curry-appl ((op (car T)) (args (cdr T)))
    (if (null? args) op
     (curry-appl (list op (car args)) (cdr args))))))
 (cond
  ((var? T) T)
  ((abstr? T) (curry-abstr T))
  ((appl? T) (curry-appl T))
  (else (error 'curry "incorrect subterm: ~s" T))))

; Evaluator expecting a checked and curried lambda term.

(define (eval-aux T E)
 (if (symbol? T) (lookup T E)
  (if (= (length T) 3)
   (let ((body (caddr T)) (var (caadr T)))
    (λ (arg) (eval-aux body (extend-env var arg E))))
   (let ((op (eval-aux (car T) E)) (arg (eval-aux (cadr T) E)))
    (if (procedure? op) (op arg)
     (error 'eval
      "op of appl must be a proc, found: ~s applied to arg: ~s"
      op arg))))))

(define (extend-env var arg E) (cons (cons var arg) E))
(define (lookup var E) (let ((B (assq var E))) (if B (cdr B) var)))

; Tests with Church numerals.

(define (number->Church n)
 (define (expt-proc f n) (if (zero? n) 'x `(,f ,(expt-proc f (sub1 n)))))
`(λ (f) (λ (x) ,(expt-proc 'f n))))

(define (Church->number Church) ((Church add1) 0))
(define (test T expect) (test-equal (Church->number (eval T)) expect))
(define (test-bool T expect) (test-equal (((eval T) #t) #f) expect))
(define (test-true  T) (test-bool T #t))
(define (test-false T) (test-bool T #f))

(define (test-error T expect)
 (test-equal (with-handlers ((exn:fail? exn-message)) (eval T) #f) expect))
  
(let ((true '(λ (x y) x)) (false '(λ (x y) y)))
 (let
  ((C-add1  '(λ (n f x) (f (n f x))))
   (C-sub1  '(λ (n f x) (n (λ (g h) (h (g f))) (λ (u )x) (λ (u) u))))
   (C-plus  '(λ (m n f x) (m f (n f x))))
   (C-mult  '(λ (m n f) (n (m f))))
   (C-expt  '(λ (m n) (n m)))
   (C-zero? `(λ (n) (n (λ (x) ,false) ,true)))
   (Y '(λ (m) ((λ (f) (m (λ (x) (f f x)))) (λ (f) (m (λ (x) (f f x))))))))
  (let
   ((C-minus `(λ (m n) (n ,C-sub1 m)))
    (C-eq?
    `(,Y ; for recursion
      (λ (eq n m)
       ; write (if test a b) as ((test (λ (x) a) (λ (x) b)) ignore)
       ; in order to delay the evaluation of the alternatives a and b.
       ; (λ (n m)
       ;  (if (C-zero? n) (C-zero? m)
       ;   (if C-zero? m) false
       ;    (eq (C-sub1 n) (C-sub1 m))))) becomes:
       (((,C-zero? n)
         (λ (x) (,C-zero? m))
         (λ (x)
          (((,C-zero? m)
            (λ (x) ,false)
            (λ (x) (eq (,C-sub1 n) (,C-sub1 m))))
           ignore)))
        ignore)))))
   (let
    ((first-ten-Churches (build-list 10 number->Church)))
    (let-values
     (((C0 C1 C2 C3 C4 C5 C6 C7 C8 C9)
       (apply values first-ten-Churches)))
     (test-equal first-ten-Churches
     '((λ (f) (λ (x) x))
       (λ (f) (λ (x) (f x)))
       (λ (f) (λ (x) (f (f x))))
       (λ (f) (λ (x) (f (f (f x)))))
       (λ (f) (λ (x) (f (f (f (f x))))))
       (λ (f) (λ (x) (f (f (f (f (f x)))))))
       (λ (f) (λ (x) (f (f (f (f (f (f x))))))))
       (λ (f) (λ (x) (f (f (f (f (f (f (f x)))))))))
       (λ (f) (λ (x) (f (f (f (f (f (f (f (f x))))))))))
       (λ (f) (λ (x) (f (f (f (f (f (f (f (f (f x)))))))))))))
     (test C0 0)
     (test C1 1)
     (test C2 2)
     (test C3 3)
     (test C4 4)
     (test C5 5)
     (test C6 6)
     (test C7 7)
     (test C8 8)
     (test C9 9)
     (test       `(,C-add1  ,C7    )                 8)
     (test       `(,C-sub1  ,C7    )                 6)
     (test       `(,C-sub1  ,C0    )                 0)
     (test       `(,C-plus  ,C5 ,C7)                12)
     (test       `(,C-mult  ,C5 ,C7)                35)
     (test       `(,C-minus ,C5 ,C5)                 0)
     (test       `(,C-minus ,C7 ,C5)                 2)
     (test       `(,C-expt  ,C7 ,C3)               343)
     (test       `(,C-expt  ,C5 ,C4)               625)
     (test       `(,C-expt  ,C5 (,C-expt ,C2 ,C2)) 625)
     (test-false `(,C-eq?   ,C7 ,C5))
     (test-false `(,C-eq?   ,C5 ,C7))
     (test-true  `(,C-eq?   ,C0 ,C0))
     (test-true  `(,C-eq?   ,C1 ,C1))
     (test-true  `(,C-eq?   ,C7 ,C7))
     (test-true  `(,C-zero? (,C-minus ,C5 ,C5)))
     (test-true  `(,C-zero? (,C-minus ,C5 ,C8)))
     (test-false `(,C-zero? (,C-minus ,C5 ,C4)))
     (test-error '(a b)
      "eval: op of appl must be a proc, found: a applied to arg: b")
     (test-error 1          "curry: incorrect subterm: 1")
     (test-error 'λ         "curry: incorrect subterm: λ")
     (test-error '(λ (a) λ) "curry: incorrect subterm: λ")
     (test-error '(λ (a) λ) "curry: incorrect subterm: λ")
     (test-error '(λ (λ) a) "curry: incorrect subterm: λ")
     (test-error '(a . b)   "curry: incorrect subterm: (a . b)")
     )))))

(test-results)