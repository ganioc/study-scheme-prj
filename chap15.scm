;; chap15 stream?
;;

(load "util.scm")
;; (load "chap14.scm")

(define random-2-to-12-list
  (lambda (n)
    (if (zero? n)
	'()
	(cons (+ 2 (random 11))
	      (random-2-to-12-list (sub1 n))))
    ))

(define sum-until-first-7
  (letrec
      ((local-sum
	(lambda (r1 sum count)
	  (if (null? r1)
	      (writeln
	       "A seven was not found ; sum= "
	       sum " and count = " count)
	      (let ((next (car r1)))
		(if (= next 7)
		    (writeln "sum = " sum " when count = " count)
		    (local-sum
		     (cdr r1)
		     (+ next sum)
		     (add1 count))))))))
    (lambda (rand-list)
      (local-sum rand-list 0 0))))

(define fib
  (lambda (n)
    (if (< n 2)
	n
	(+ (fib (- n 1)) (fib (- n 2))))
    ))

;; page 505
(define the-null-delayed-list '())

(define del-list
  (cons (fib 8)
	(delay (cons (fib 9)
		     (delay '())))))

(define delayed-list-car
  (lambda (x)
    (car x)))

;; (define delayed-list-cdr
;;   (lambda (x)
;;     (force (cdr x))
;;     ))
(define delayed-list-cdr (compose force cdr))

(define delayed-list-null?
  (lambda (delayed-list)
    (null? delayed-list)))


;; test ---------------------------------------
;; (display 'test )
(random-2-to-12-list 10)

(display (fib 1))
(display (fib 2))
(display (fib 3))
(display (fib 4))
(display (fib 5))
(display (fib 6))
(display (fib 7))
(display (fib 8))
(display (fib 9))

(delayed-list-car del-list)

(cdr del-list)
(car ((force (cdr del-list))))
(display 'test-delayed-list-car)
(delayed-list-car ((delayed-list-cdr del-list)))
;; (car (force (cdr del-list)))
(delayed-list-null? ((delayed-list-cdr ((delayed-list-cdr del-list)))))
((force (delay 'a)))


;; (sum-until-first-7 (random-2-to-12-list 100))
;; ;; But a list of 100 random numbers was generated, only 4 were used
;; ;; Is there any way to create a list , until we are ready to process it
;; (writeln "----------------------------------------")
;; ;; delayed list, 
;; (display del-list)
;; ;; (delayed-list-car (delayed-list-cdr del-list))
;; (delayed-list-cdr del-list)
;; (delayed-list-car (delayed-list-cdr del-list))
