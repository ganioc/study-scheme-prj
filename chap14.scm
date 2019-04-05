;;
(load "util.scm")

(define feeze
  (lambda (expr)
    (lambda () expr)))

(define sm (feeze (+ 3 4)))

;; macro test
(define freeze-transformer
  (lambda (code)
    (cons 'lambda (cons '() (list (2nd code))))
    ))

;; (macro freeze
;;   (lambda (code)
;;     (cons 'lambda (cons '() (list (2nd code))))
;;     ))

;; (extend-syntax (freeze)
;; 	       ((freeze expr1 expr2)
;; 		(lambda () expr1 expr2 )))

(define-syntax swap
  (syntax-rules ()
    ((swap a b) (let ((tmp b))
		  (set! b a)
		  (set! a tmp)))
    ))

(define-syntax kwote
  (syntax-rules ()
    ((kwote exp)
     (quote exp))))

(kwote (foo . bar))

(writeln (kwote (foo . bar)))
;; lazy evaluation
(define-syntax freeze
  (syntax-rules ()
    ((freeze expr1 expr2 ...)
     (lambda () expr1 expr2 ...))
    ))

;; thawing
(define thaw
  (lambda (thunk)
    (thunk)))

(define make-promise "procedure")
(define force "procedure")

(let ((delayed-tag "delay")
      (value-tag "-->"))
  (set! make-promise (lambda (thunk)
		       (cons delayed-tag thunk)))
  (set! force
	(lambda (arg)
	  (if (and (pair? arg) (eq? (car arg) delayed-tag))
	      (begin
		(set-car! arg value-tag)
		(set-car! arg (thaw (cdr arg)))))
	  (cdr arg))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr1 expr2 ...) (make-promise
			      (freeze expr1 expr2 ...)))))

(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) expr1 expr2 ...)
     ((lambda (var ...) expr1 expr2 ...) val ...))))
;; var to val, one to one correspondence

(define-syntax my-letrec
  (syntax-rules ()
    ((my-letrec ((var val) ...) expr1 expr2 ...)
     (let ((var "any") ...)
       (set! var val) ...
       expr1 expr2 ...))))

(define or-proc
  (lambda (th-list)
    (cond
     ((null? th-list) #f)
     (else (let ((v (thaw (car th-list))))
	     (if v v (or-proc (cdr th-list))))))
    ))

(define or-transformer
  (lambda (code)
    (list 'or-proc
	  (cons 'list
		(map (lambda (e) (list 'freeze e))
		     (cdr code))))))

(define-syntax or
  (syntax-rules ()
    ((or code ...) (or-proc (list (freeze code) ...)))
    ))



;; test ---------------------------------------------
(define num1 100)
(define num2 102)

(writeln num1 ", " num2)
(newline)

;; (display "after swap")
(swap num1 num2)

(writeln num1 ", "  num2)
(newline)

(define th (freeze (display "A random number is: ")
		   (display (random 10))))

(thaw th)
(thaw th)
(thaw th)
(thaw th)

(display 'make-promise)
(define pr (delay (display "A random number is: ")
		  (random 10)))

(force pr)
(force pr)
(force pr)

(define number-a
  (my-let
   ((a 1)
    (b 10))
   a))

(display number-a)

(display "To test my-odd?")
(define my-odd?
  (my-letrec
   ((even? (lambda (n)
	     (if (zero? n)
		 #t
		 (odd? (sub1 n)))))
    (odd? (lambda (n)
	    (if (zero? n)
		#f
		(even? (sub1 n))))))
   odd?))

(my-odd? 2)
(my-odd? 19)

(freeze-transformer '(freeze (cons 'a '(b c))))

 





