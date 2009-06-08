#lang scheme

;;; Derivation of an applicative order Y combinator.
;
; We want to make function factorial. The first attempt is:

;(let*
; ((factorial (lambda (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
; (factorial 4))

; This definition is wrong, because the highlighted occurrence of variable factorial is not bound. Having lambda as the only binding form, the variable can be bound only as in (λ (factorial) (λ (n) ...)) This function makes procedure factorial given function factorial. 

;(let*
; ((make-factorial
;   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
;  (factorial (make-factorial ???)))
; (factorial 4))

; What can we put in place of the question marks? We can't replace them by factorial, for this variable is not yet bound. But we have factorial-maker.

;(let*
; ((make-factorial
;   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
;  (factorial (make-factorial make-factorial)))
; (factorial 4))

; This obviously is wrong. Procedure make-factorial is given procedure make-factorial, not factorial itself. Therefore:

(let*
 ((make-factorial
   (λ (make-factorial)
    (λ (n) (if (zero? n) 1 (* n ((make-factorial make-factorial) (sub1 n)))))))
  (factorial (make-factorial make-factorial)))
 (factorial 4)) ; --> 24

; This works. We have a function make-factorial that given itself for its argument produces procedure factorial. Notice that within
; (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))
; we had to replace variable factorial by
; (make-factorial make-factorial)
; which we can call factorial:

;(let*
; ((make-factorial
;   (λ (make-factorial) ; <--
;    (let* ((factorial (make-factorial make-factorial)))
;     (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))))
;  (factorial (make-factorial make-factorial)))
; (factorial 4))

; The latest trial is commented out. Can you see why? Copy the trial in a separate definitions window, uncomment it and then run Debug. What we see is that the function starting at the third line (marked with <--) tries to form procedure factorial in the inner let*-form but this causes procedure make-factorial to call itself again, which causes procedure make-factorial to call itself again, and so on. We have to postpone the evaluation of (make-factorial make-factorial) in the inner let*-form. The value is intended to be function factorial, which has one argument, namely n. We always can replace factorial by
; (λ (n) (factorial n)) or rather by
; (λ (n) ((make-factorial make-factorial) n))
; This indeed postpones the evaluation of (make-factorial make-factorial) until it value is needed.

(let*
 ((make-factorial
   (λ (make-factorial)
    (let* ((factorial (λ (n) ((make-factorial make-factorial) n))))
     (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))))
  (factorial (make-factorial make-factorial)))
 (factorial 4))

; Now expand the inner let*-form in terms of lambda:

(let*
 ((make-factorial
   (λ (make-factorial)
    ((λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
     (λ (n) ((make-factorial make-factorial) n)))))
  (factorial (make-factorial make-factorial)))
 (factorial 4))

; Now (λ (factorial) (λ (n) ...)) does not refer to make-factorial. Therefore it can be lifted out of function make-factorial:

(let*
 ((other-make-factorial
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (make-factorial
   (λ (make-factorial)
    (other-make-factorial (λ (n) ((make-factorial make-factorial) n)))))
  (factorial (make-factorial make-factorial)))
 (factorial 4))

; In the second line from below make-factorial is applied to itself. In this line we can replace variable make-factorial by its definition in the second binding clause of the let*form:

(let*
 ((other-make-factorial
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (factorial
   ((λ (make-factorial)
     (other-make-factorial (λ (n) ((make-factorial make-factorial) n))))
    (λ (make-factorial)
     (other-make-factorial (λ (n) ((make-factorial make-factorial) n)))))))
 (factorial 4))

; Now abstract the definition of function factorial for variable other-make-factorial:

(let*
 ((other-make-factorial
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (yet-other-make-factorial
   (λ (other-make-factorial)
    ((λ (make-factorial)
      (other-make-factorial (λ (n) ((make-factorial make-factorial) n))))
     (λ (make-factorial)
      (other-make-factorial (λ (n) ((make-factorial make-factorial) n)))))))
  (factorial (yet-other-make-factorial other-make-factorial)))
 (factorial 4))

; Function yet-other-make-factorial has no free variables. We also can use for the preparation of other recursive functions like in:

(let*
 ((make-length
   (λ (length) (λ (x) (if (null? x) 0 (add1 (length (cdr x)))))))
  (yet-other-make-factorial
   (λ (other-make-factorial)
    ((λ (make-factorial)
      (other-make-factorial (λ (n) ((make-factorial make-factorial) n))))
     (λ (make-factorial)
      (other-make-factorial (λ (n) ((make-factorial make-factorial) n)))))))
  (length (yet-other-make-factorial make-length)))
 (length '(a b c)))

; Therefore we can generalize the name of choice of the yet-other-make-factorial and the variables inside it:

(let*
 ((other-make-factorial
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (Y
   (λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n)))))))
  (factorial (Y other-make-factorial)))
 (factorial 4))

; Now we have only one factorial maker left. hence other-factorial-maker can be called facorial-maker:

(let*
 ((make-factorial
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (Y
   (λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n)))))))
  (factorial (Y make-factorial)))
 (factorial 4))

; Now replace Y and make-factorial in the second line from below by their definitions:

(let*
 ((factorial
   ((λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n))))))
    (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))))
 (factorial 4))

; Which can be written as:

(((λ (m)
   ((λ (f) (m (λ (n) ((f f) n))))
    (λ (f) (m (λ (n) ((f f) n))))))
  (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
 4)

; Function Y is an applicative order Y combinator. It is a fixed point combinator. We have:

; (Y (λ (arg) ‹body›)) = ((λ (arg) ‹body›) (Y (λ (arg) ‹body›)))

; Let's try this:

(let*
 ((make-factorial
   (λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n)))))))
  (Y
   (λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n)))))))
  (factorial (make-factorial (Y make-factorial))))
 (factorial 4))

; In order to prove that
; (Y (λ (arg) ‹body›)) = ((λ (arg) ‹body›) (Y (λ (arg) ‹body›)))
; We shall need a notation for the value of expression. This value depends on the environment of the expression, it est the bindings of all free variables of the expression. We write [expresssion env ((‹variable› ‹value›) ...)]

[(Y (λ (x) body)) ()]

apply [Y ()] to [(λ (x) body) ()]

apply [(λ (m) ((λ (f) (m (λ (n) ((f f) n)))) (λ (f) (m (λ (n) ((f f) n)))))) ()]
   to [(λ (x) body) ()]

[((λ (f) (m (λ (n) ((f f) n)))) (λ (f) (m (λ (n) ((f f) n)))))
 ((m [(λ (x) body) ()]))]

apply [(λ (f) (m (λ (n) ((f f) n)))) ((m [(λ (x) body) ()]))]
   to [(λ (f) (m (λ (n) ((f f) n)))) ((m [(λ (x) body) ()]))]

[(m (λ (n) ((f f) n)))
 ((f [(λ (f) (m (λ (n) ((f f) n)))) ((m [(λ (x) body) ()]))])
  (m [(λ (x) body) ()]))]

apply [(λ (x) body) ()]
   to [(λ (n) ((f f) n))
       ((f [(λ (f) (m (λ (n) ((f f) n)))) ((m [(λ (x) body) ()]))]))]

[body
 ((x [(λ (n) ((f f) n))
      ((f [(λ (f) (m (λ (n) ((f f) n)))) ((m [(λ (x) body) ()]))]))]))]

[body
 ((x [





apply (λ (m) ((λ (f) (m (λ (n) ((f f) n)))) (λ (f) (m (λ (n) ((f f) n)))))
       env ()) to (λ (x) body env ())

$((λ (f) (m (λ (n) ((f f) n)))) (λ (f) (m (λ (n) ((f f) n)))))

; Here make-factorial is a function that returns a function. It may as well return something else. 

(let*
 ((make-endless-list (λ (make-endless-list) (λ (x) (cons ))
  (Y
   (λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n))))))))
 (Y make-something))

; And:

(let*
 ((make-something (λ (something) something))
  (Y
   (λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n))))))))
 (make-something (Y make-something)))

; And:

(let*
 ((make-something (λ (something) something))
  (Y
   (λ (m)
    ((λ (f) (m (λ (n) ((f f) n))))
     (λ (f) (m (λ (n) ((f f) n))))))))
 (make-something (make-something (Y make-something))))











;(define (factorial factorial-arg n)
; (if (zero? n) 1 (* n (factorial-arg factorial-arg (sub1 n)))))
;
;(factorial factorial 3)
;
;; Let's curry function factorial and rename it factorial-maker, for it becomes a function that returns function factorial.
;
;(define factorial-maker
; (λ (make-factorial-arg)
;  (λ (n)
;   (if (zero? n) 1 (* n ((make-factorial-arg make-factorial-arg) (sub1 n)))))))
;
;((factorial-maker factorial-maker) 3)
;
;; We don't need define, for facorial-maker is not recursive. We can use a let* form.
;
;(let*
; ((make-factorial
;   (λ (make-factorial-arg)
;    (λ (n)
;     (if (zero? n) 1
;      (* n ((make-factorial-arg make-factorial-arg) (sub1 n))))))))
; ((make-factorial make-factorial) 3))
;
;; Now expand the let* form in terms of lambda.
;
;((λ (make-factorial) ((make-factorial make-factorial) 3))
; (λ (make-factorial-arg)
;  (λ (n)
;   (if (zero? n) 1
;    (* n ((make-factorial-arg make-factorial-arg) (sub1 n)))))))
;
;; (make-factorial-arg make-factorial-arg) is function factorial. Hence we had better give it this name. The same holds for
;; (make-factorial-arg make-factorial-arg
;
;;((λ (make-factorial)
;;  (let* ((factorial (make-factorial make-factorial)))
;;   (factorial 3)))
;; (λ (make-factorial-arg)
;;  (let* ((factorial (make-factorial-arg make-factorial-arg)))
;;   (λ (n)
;;    (if (zero? n) 1
;;     (* n (factorial (sub1 n))))))))
;
;; There is good reason why the last expression is commented out. Can you see why? You may want to copy it in a fresh definitions window, uncomment it and then run Debug on it. Procedure (λ (make-factorial-arg) etc) is applied to itself. When applied the let*-form requires evaluation of
;; (make-factorial-arg make-factorial-arg)
;; Again the let*-form requires evaluation of the same expression. We have a tight loop here. In order to prevent the loop we must postpone the evaluation of
;; (make-factorial-arg make-factorial-arg)
;; This expression is intended to produce a function of one argument. Therefore we can wrap it as:
;; (λ (n) ((make-factorial-arg make-factorial-arg) n)
;
;((λ (make-factorial)
;  (let* ((factorial (make-factorial make-factorial)))
;   (factorial 3)))
; (λ (make-factorial-arg)
;  (let* ((factorial (λ (n) ((make-factorial-arg make-factorial-arg) n))))
;   (λ (n)
;    (if (zero? n) 1
;     (* n (factorial (sub1 n))))))))
;
;; Now expand both let*-forms in terms of lambda.
;
;((λ (make-factorial)
;  ((λ (factorial) (factorial 3))
;   (make-factorial make-factorial)))
; (λ (make-factorial-arg)
;  ((λ (factorial) (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
;   (λ (n) ((make-factorial-arg make-factorial-arg) n)))))
;
;; This can be transformed to:
;
;((λ (factorial)
;   
;
;
; 
;    