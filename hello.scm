;first Program

(begin
  (display "Hello World")
  (newline)
  1))

(boolean? #t)
(not #t)

(max 2 3 4)
(display `E)
(symbol? `xyz)

(define xyz 19)
(display xyz)

(vector 0 1 2 3 4 5)
(define v1 (make-vector 5))
(vector-set! v1 0 90)
(vector-set! v1 1 91)
(vector-set! v1 2 92)
(vector-set! v1 3 93)
(vector-set! v1 4 94)

(display v1)
(cons 1 #t)
`(1 . #t)
(define x (cons 1 2))
(car x)
(cdr x)
(set-car! x 10)
(set-cdr! x 12)
x
(define L (list 4 5 6 7))
(display L)
(display (null? `()))
(display (char-downcase #\2))
(char->integer #\d)
(integer->char 50)
(string->list "hello")
(number->string 16)
(string->number "16" 8)
(symbol->string `symbol)
(string->symbol "sting")
cons
(display "hello" (current-output-port))
((lambda (x) (+ x 2)) 3)
(apply + 2 3 4)

; Test closure
(define counter 0)
(define bump-counter
  (lambda ()
    (set! counter (+ counter 1))
    counter))
(bump-counter)
(bump-counter)

; , let*
(let ((x 1)
      (y 2)
      (z 3))
  (list x y z))
(let* ((x 1)
       (y x))
  (+ x y))

(let ((cons (lambda (x y) (* x y))))
  (cons 2 4))

; recursion
(define factorial
  (lambda (n)
    (if (= n 0) 1
	(* n (factorial (- n 1))))))

(factorial 5)

; letrec

(letrec ((countdown (lambda (i)
		      (if (= i 0) `liftoff
			  (begin
			    (display i)
			    (newline)
			    (countdown (- i 1))
			    )))))
  (countdown 10))

(let countdown ((i 12))
  (if (= i 0) `liftoff-2nd
      (begin
	(display i)
	(newline)
	(countdown (- i 1)))))

; iteration

(define list-position
  (lambda (o l)
    (let loop ((i 0)
	       (l 2))
      (if (null? l) #f
	  (if (eqv? (car l) o) i
	      (loop (+ i 1) (cdr l)))))))

; reverse a list,好绕啊，这个思路!

(define reverse!
  (lambda (s)
    (let loop ((s s) (r `()))
      (if (null? s) r
	  (let ((d (cdr s)))
	    (display (cons  "s:" s))
	    (display (cons "r:" r))
	    (display (cons "d:" d))
	    (set-cdr! s r)
	    (display (cons "s:" s))
	    (newline)
	    (loop d s))))))

(reverse! `(a b c))
(define STR `st)
(display STR)

(define add2 (lambda (x)
	       (+ x 2)))
(map add2 `(1 2 3))
(for-each display (list "one " "two "))
(map cons `(1 2 3) `(10 20 30))
(map + `(1 2 3) `(10 20 30))

(write `yaan)
(write-char #\7)

; file io

(define i (open-input-file "./hello.txt"))
(read-char i)
(define j (read i))
j
(close-input-port i)


(define outf (open-output-file "./greeting.txt"))
(display "hello world" outf)
(write-char #\space outf)
(display "W昂国威" outf)
(newline outf)

(close-output-port outf)

(define o (open-output-string))
(write 'hello o)
(write-char #\space o)
(newline o)
(write 'world o)
(get-output-string o)

;; macro

;; (define-macro when2
;;   (lambda (test branch)
;;     (list 'if test
;; 	  (display branch))))
	  
;; (when2 (< 20 69)
;;        (display 'small)
;;        (display 'big-equal)
;;        (display 'big))

;; a

;; (list `if test
;;       (cons `begin branch))

(display 'Xinhai)

;; macro capture

(define-syntax my-or
  (syntax-rules ()
    ((my-or x y) (if x x y))))
(my-or #f 3)
(my-or 0 3)

;; structure
;; (defstruct tree height age)
;; (define coconut
;;   (make-tree 'height 30
;; 	     'age 100))
;; (tree.height coconut)

;;
;;(system "ls -l")
;;(getenv "HOME")












