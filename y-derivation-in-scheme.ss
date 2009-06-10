#lang scheme ; File y-derivation-in-scheme.ss

#|Preparation of a recursive function without define-forms, letrec-forms names let-forms or any other form allowing recursive variable references. First look at the usual way to define function factorial:|#

; Version 1

(define factorial-1 (λ (n) (if (zero? n) 1 (* n (factorial-1 (sub1 n))))))

(factorial-1 4)

#|Add a layer of abstraction:|#

; Version 2

(define make-factorial-2
 (λ ()
  (λ (n) (if (zero? n) 1 (* n ((make-factorial-2) (sub1 n)))))))

(define factorial-2 (make-factorial-2))
(factorial-2 4)

#|Function factorial-2 has no argument, but we can give it a dummy argument. Why we would want to do so will become clear in version 5.|#

; Version 3

(define make-factorial-3
 (λ (dummy-arg)
  (λ (n) (if (zero? n) 1 (* n ((make-factorial-3 dummy-arg) (sub1 n)))))))

(define factorial-3 (make-factorial-3 "dummy"))
(factorial-3 4)

#|In version 3 the dummy argument is never used. It is just passed around. It does not matter what actual argument we provide for the dummy formal argument. We may as well pass make-factorial-3 to itself. Why we want to do that? This will become clear within two more steps. The dummy argument can be renamed such as to indicate that its value will be the factorial-maker itself.|#

; Version 4

(define make-factorial-4
 (λ (make-factorial)
  (λ (n) (if (zero? n) 1 (* n ((make-factorial-4 make-factorial) (sub1 n)))))))

(define factorial-4 (make-factorial-4 make-factorial-4))
(factorial-4 4)

#|In the definition of make-factorial-4 we have subexpression (make-factorial-4 make-factorial) but make-factorial-4 is called with itself for its actual argument 'make-factorial' The two variables have the same value. Hence we can replace (make-factorial-4 make-factorial) by (make-factorial make-factorial):|#

; Version 5

(define make-factorial-5
 (λ (make-factorial)
  (λ (n) (if (zero? n) 1 (* n ((make-factorial make-factorial) (sub1 n)))))))

(define factorial-5 (make-factorial-5 make-factorial-5))
(factorial-5 4)

#|Now we see why it made sense to apply make-factorial to itself. make-factorial-5 no longer is recursive, or is it? Anyway, function (λ (make-factorial) ...) no longer has a recursive reference to make-factorial-5.

Function (λ (n) ...) contains the subexpression (make-factorial make-factorial), where we would like to write 'factorial' This can be done by binding the value of the subexpression to a variable called 'factorial'.|#

;; Version 6
;
;(define make-factorial-6
; (λ (make-factorial)
;  ((λ (factorial)
;    (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
;   (make-factorial make-factorial))))
;
;(define factorial-6 (make-factorial-6 make-factorial-6))
;(factorial-6 4)

#|Version 6 has been commented out for good reason. Copy the example into a fresh definitions window and use Debug to see what happens. Function make-factorial-6 immediately applies make-factorial to itself. But we have given make-factorial-6 for make-factorial. Hence make-factorial-6 is applied to itself, but this application makes make-factorial-6 apply itself to itself immediately again and again and again ... What has happend during the transformation from version 5 to version 6? In version 5 the self application (make-factorial make-factorial) is in the else-part of an if-form. It is evaluated only when needed. At the bottom of the recursion, it is not evaluated. However, in version 6 the self-application is evaluated unconditionally for it has been lifted out of the if-form. We have to postpone the self application until it is needed in the else-part of the if-form. This can be done by wrapping it in a lambda-form. If we have an expression F that produces a function of one argument, the function (λ (x) (F x)) exactly does the same, provided F produces no side effects and does not refer to variable x. If F does depend on a variable called 'x', we can take (λ (y) (F y)) or choose any other formal argument that does not conflict with the variables used in F. F is not evaluated before (λ (x) (F x)) is called. That is precisely what a function is for. It is a recipe. We don't have to start cooking right away. We can wait until we are hungry and that is in the else-part if the if-form.|#

; Version 7

(define make-factorial-7
 (λ (make-factorial)
  ((λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
   (λ (n) ((make-factorial make-factorial) n)))))

(define factorial-7 (make-factorial-7 make-factorial-7))
(factorial-7 4)

#|Now we can put the definition of make-factorial-7 into the self application that defines factorial-7:|#

; Version 8

(define factorial-8
 ((λ (f) (f f))
  (λ (make-factorial)
   ((λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
    (λ (n) ((make-factorial make-factorial) n))))))

(factorial-8 4)

#|Function (λ (factorial) ...) Does not refer to make-factorial. Therefore it can lifted to out. We call it m.|#

; Version 9

(define factorial-9
 ((λ (m)
   ((λ (f) (f f))
    (λ (make-factorial) (m (λ (n) ((make-factorial make-factorial) n))))))
  (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))))

(factorial-9 4)

#|Now function (λ (m) ...) is independent from the algorithm used for the factorial function. It can be used for other functions too. Therefore we change the name make-factorial to a more general one. We choose the name g.|#

; Version 10

(define factorial-10
 ((λ (m) ((λ (f) (f f)) (λ (g) (m (λ (n) ((g g) n))))))
  (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))))

(factorial-10 4)

(define length-10
 ((λ (m) ((λ (f) (f f)) (λ (g) (m (λ (n) ((g g) n))))))
  (λ (length) (λ (lyst) (if (null? lyst) 0 (add1 (length (cdr lyst))))))))

(length-10 '(a b c d e f g h i j k l m n o p q r s t u v w x))
   
#|Now we don't need define:|#

;Version 11

(((λ (m) ((λ (f) (f f)) (λ (g) (m (λ (n) ((g g) n))))))
  (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
 4)

(((λ (m) ((λ (f) (f f)) (λ (g) (m (λ (n) ((g g) n))))))
  (λ (length) (λ (lyst) (if (null? lyst) 0 (add1 (length (cdr lyst)))))))
'(a b c d e f g h i j k l m n o p q r s t u v w x))

#|Function (λ (m) ...) is usually called Y, in this case an applicative order Y combinator. Applicative order, because it is suited to applicative order evaluation as in Scheme. This means that an actual argument is evaluated before its value is passed to the function that needs it. It also is important that a function does not evaluate its body until it is called. Well it can't, because it needs values for its formal arguments. However it must not even evaluate parts of its body that do not depend on the formal arguments, unless an optimizer can prove that the evaluation does no harm. Y is called a combinator because it only contains references to variables that are bound within Y. That is the definition of a combinator.|#

; Version 12
(let*
 ((Y (λ (m) ((λ (f) (f f)) (λ (g) (m (λ (n) ((g g) n)))))))
  (factorial (Y (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))))
 (factorial 4))

(let*
 ((Y (λ (m) ((λ (f) (f f)) (λ (g) (m (λ (n) ((g g) n)))))))
  (length (Y (λ (length) (λ (lst) (if (null? lst) 0 (add1 (length (cdr lst)))))))))
 (length '(a b c d e f g h i j k l m n o p q r s t u v w x)))

#|Y as used in version 12 is not the only possible way to write it. For example Y can also be written as:|#

; Version 13

(define Y-13
 (λ (m)
  ((λ (f) (m (λ (x) ((f f) x))))
   (λ (f) (m (λ (x) ((f f) x)))))))

((Y-13 (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))) 4)

((Y-13 (λ (length) (λ (lst) (if (null? lst) 0 (add1 (length (cdr lst)))))))
'(a b c d e f g h i j k l m n o p q r s t u v w x))

#|or as:|#

; Version 14

(define Y-14
 ((λ (x) (x x x x x x x x x x x x x x x x x x x x x x x x x x x))
  (λ (a b c d e f g h i j k l m n o p q r s t u v w x y z)
   (λ (u)
    (λ (v)
     ((u ((t h i s i s a f i x e d p o i n t c o m b i n a t o r) u)) v))))))

((Y-14 (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))) 4)

((Y-14 (λ (length) (λ (lst) (if (null? lst) 0 (add1 (length (cdr lst)))))))
'(a b c d e f g h i j k l m n o p q r s t u v w x))

#|or as:|#

; Version 15

(define Y-15
 ((λ (x) (x x x x x x x x x x x x x x x x x x x x x x x x x x))
  (λ (a b c d e f g h i j k l m n o p q s t u v w x y z)
   (λ (u) (λ (v) ((u ((x x x x x x x x x x x x x x x x x x x x x x x x x x) u)) v))))))
                      
((Y-15 (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))) 4)

((Y-15 (λ (length) (λ (lst) (if (null? lst) 0 (add1 (length (cdr lst)))))))
'(a b c d e f g h i j k l m n o p q r s t u v w x))

#|When you run this program, all results should be 24.|#

