#lang scheme

Consider the function:

(λ (m)
 ((λ (f) (m (λ (x) ((f f) x))))
  (λ (f) (m (λ (x) ((f f) x))))))

It is called the applicative-order Y-combinator and we shall use symbol 'Y' as an abbreviation. Now consider:

$(Y (λ (x) ‹body›))
=
$((λ (f) (m (λ (x) ((f f) x))))
  (λ (f) (m (λ (x) ((f f) x)))))
where $m = $(λ (x) ‹body›)
=
$(m (λ (x) ((f f) x))
where $m = $(λ (x) ‹body›)
and $f = $(λ (f) (m (λ (x) ((f f) x))))
=
$($(λ (x) ‹body›) (λ (x) ((f f) x)))
where $f = $(λ (f) (m (λ (x) ((f f) x))))
=
$‹body›
where $x = $(λ (x) ((f f) x))
and $f = $(λ (f) (m (λ (x) ((f f) x))))
=
$‹body›
where $x = $(λ (x)
             (($(λ (f) (m (λ (x) ((f f) x)))
               $(λ (f) (m (λ (x) ((f f) x))))
              x)))
which does the same as
$‹body› where $x = $(Y (λ (x) ‹body›))
|#
#|
Let's try to make the factorial function without define- or letrec-forms. In the body of the function its name is required. In the Lambda Calculus the only way to bind a name is lambda. Hence we give the name as an argument to the function:

(λ (factorial n) (if (zero? n) 1 (* n (factorial factorial (sub1 n)))))

Function factorial requires two arguments itself and a number. Let's try:
|#
((λ (factorial n) (if (zero? n) 1 (* n (factorial factorial (sub1 n)))))
 (λ (factorial n) (if (zero? n) 1 (* n (factorial factorial (sub1 n)))))
 3)
#|
In order to make function factorial itself we could write:
|#
(let
 ((factorial
   (λ (n)
    ((λ (factorial n) (if (zero? n) 1 (* n (factorial factorial (sub1 n)))))
     (λ (factorial n) (if (zero? n) 1 (* n (factorial factorial (sub1 n)))))
     n))))
 (factorial 3))
#|
However, more convenient is:
|#
(let*
 ((make-factorial
   (λ (make-factorial)
    (λ (n) (if (zero? n) 1 (* n ((make-factorial make-factorial) (sub1 n)))))))
  (factorial (make-factorial make-factorial)))
 (factorial 3))
#|
This can be rewritten as:
|#
(let*
 ((make-factorial
   (λ (make-factorial)
    (let ((factorial (λ (n) ((make-factorial make-factorial) n))))
     (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))))
  (factorial (make-factorial make-factorial)))
 (factorial 3))
#|
Notice that in the inner let-form we wrote
(λ (n) ((make-factorial make-factorial) n))
in stead of:
(make-factorial make-factorial)
This is necessary, because otherwise a tight loop of calling make-factorial itself would be obtained. The evaluation of (make-factorial make-factorial) must be postponed until it is needed. In the preceding versions (make-factorial make-factorial) was within the if-form and therefore not evaluated when n becomes zero. Expanding the inner let-form in terms of lambda yields:
|#
(let*
 ((make-factorial
   (λ (make-factorial)
     ((λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
      (λ (n) ((make-factorial make-factorial) n)))))
  (factorial (make-factorial make-factorial)))
 (factorial 3))
#|
Now (λ (factorial) (λ (n) ...)) can be lifted out of procedure make-factorial:
|#
(let*
 ((factorial-maker
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (make-factorial
   ((λ (factorial-maker)
     (λ (make-factorial)
      (factorial-maker (λ (n) ((make-factorial make-factorial) n)))))
    factorial-maker))
  (factorial (make-factorial make-factorial)))
 (factorial 3))
#|
We can move the self application of make-factorial in the last but one line into procedure make-factorial:
|#
(let*
 ((factorial-maker
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (make-factorial
   (λ (factorial-maker)
    ((λ (make-factorial)
      (factorial-maker (λ (n) ((make-factorial make-factorial) n))))
     (λ (make-factorial)
      (factorial-maker (λ (n) ((make-factorial make-factorial) n)))))))
  (factorial (make-factorial factorial-maker)))
 (factorial 3))
#|
Now procedure make-factorial can be used for other function makers too. Let's generalize the names of its variables. The usual name for the latest version of function make-factorial is Y or in full: Applicative-order Y-combinator.
|#
(let*
 ((factorial-maker
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (Y
   (λ (fun-maker)
    ((λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg))))
     (λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg)))))))
  (factorial (Y factorial-maker)))
 (factorial 3))
#|
Or
|#
(let*
 ((Y
   (λ (fun-maker)
    ((λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg))))
     (λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg)))))))
  (factorial
   (Y (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))))
 (factorial 3))
#|
Or
|#
(((λ (fun-maker)
   ((λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg))))
    (λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg))))))
 (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
 3)
#|
As another example:
|#
(((λ (fun-maker)
   ((λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg))))
    (λ (make-fun) (fun-maker (λ (arg) ((make-fun make-fun) arg))))))
 (λ (length) (λ (lst) (if (null? lst) 0 (add1 (length (cdr lst)))))))
 '(a b c d e f))
