;; chap16

(load "util.scm")

(display "chap16")

(char->integer #\A)
(char<? #\C #\F)
(char-ci=? #\a #\A)
(string->list "Have fun")

;; chapt 17
;; (cons (if (zero? divisor)
;; 	  (error "/:" dividend "divided by zero")
;; 	  (/ divident divisor))
;;       '(a b c))

(define receiver-1
  (lambda (proc)
    (proc (list 1))))

(define receiver-2
  (lambda (proc)
    (proc (list (proc (list 2))))))

(define receiver-3
  (lambda (proc)
    (proc (list (proc (list 3 proc))))))

(define result "any value")
(define resultcc "any value")

(define writeln/return
  (lambda (x)
    (writeln x)
    x))

(define answer-maker
  (lambda (x)
    (cons 'answer-is (writeln/return x))))
;; invoke argument
(define call
  (lambda (receiver)
    (receiver writeln/return)))

;; escaper
(define *escape/thunk* "any continuation")

(define escaper
  (lambda (proc)
    (lambda (x)
      (*escape/thunk* (lambda ()
			(proc x))))))




;; tests
(display "--------------------------")
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! result (answer-maker
	      (call receiver-1)))
result

(set! resultcc (answer-maker
		(call-with-current-continuation receiver-1)))

resultcc

(set! result (answer-maker (call receiver-2)))

result

(set! resultcc (answer-maker (call-with-current-continuation receiver-2)))

resultcc

(writeln/return 2)

(set! result (answer-maker (call receiver-3)))

result

((2nd (2nd result)) (list 1000))

(set! resultcc (answer-maker (call-with-current-continuation receiver-3)))

resultcc

((3rd resultcc) (list 1000))

resultcc

(define receiver-4
  (lambda (continuation)
    (set! *escape/thunk* continuation)
    (*escape/thunk* (lambda ()
		      (writeln "escaper is defined")))))

((call-with-current-continuation receiver-4))

(*escape/thunk* (lambda () (add1 6)))
(+ 5 (*escape/thunk* (lambda () (add1 6))))

(define escaper
  (lambda (proc)
    (lambda args
      (*escape/thunk*
       (lambda ()
	 (apply proc args))))
    ))















