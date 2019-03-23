;; Mutation
(load "util.scm")

(define stk '())

(define empty?
  (lambda ()
    (null? stk)))

(define top
  (lambda ()
    (if (empty?)
	(error "top: The stack is empty.")
	(car stk))))

(define print-stack
  (lambda ()
    (display "Top: ")
    (for-each (lambda (x) (display x) (display " "))
	      stk)
    (newline)))

(define push!
  (lambda (a)
    (set! stk (cons a stk))))

(define pop!
  (lambda ()
    (if (empty?)
	(error "pop!: The stack is empty.")
	(set! stk (cdr stk)))))
;; lookup
(define lookup
  (lambda (obj table success-proc failure-proc)
    (letrec
	((lookup (lambda (table)
		   (if (null? table)
		       (failure-proc)
		       (let ((pr (car table)))
			 (if (equal? (car pr) obj)
			     (success-proc pr)
			     (lookup (cdr table))))))))
      (lookup table))))

(define assoc
  (lambda (obj table)
    (lookup obj table (lambda (pr) pr) (lambda () #f))))

// put it in the table if not found
(define memoize
  (lambda (proc)
    (let ((table '()))
      (lambda (arg)
	(lookup arg table
		(lambda (pr) (cdr pr))
		(lambda ()
		  (let ((val (proc arg)))
		    (set! table (cons (cons arg val) table))
		    val)))))))
(define fib
  (lambda (n)
    (if (< n 2)
	n
	(+ (fib (- n 1))
	   (fib (- n 2))))))

(define fib-m (memoize fib))

(define memo-fib
  (memoize (lambda (n)
	     (if (< n 2)
		 n
		 (+ (memo-fib (- n 1))
		    (memo-fib (- n 2)))))))
				     
(define while-proc
  (lambda (pred-th body-th)
    (writeln 'a)))

(define for-effect-only
  (lambda (item-ignored)
    ;; "unspecified value"))
    (item-ignored)))

;; About object programming, box

(define delegate
  (lambda (obj msg)
    (apply obj msg)))

(define invalid-method-name-indicator "unknown")

(define base-object
  (lambda msg
    (case (1st msg)
      ((type) "base-object")
      (else invalid-method-name-indicator))))

(define send
  (lambda args
    (let ((object (car args))
	  (message (cdr args)))
      (let ((try (apply object message)))
	(if (eq? invalid-method-name-indicator try)
	    (error "Bad method name:" (car message)
		   "sent to object of"
		   (object 'type)
		   "type.")
	    try)))))

(define box-maker
  (lambda (init-value)
    (let ((contents init-value))
      (lambda msg
	(case (1st msg)
	  ((type) "box")
	  ((show) contents)
	  ((update!) (set! contents (2nd msg)))
	  ((swap!) (let ((ans contents))
		     (set! contents (2nd msg))
		     ans))
	  ((reset!) (set! contents init-value))
	  (else (delegate base-object msg)))))
    ))

(define box-maker2
  (lambda (init-value)
    (let ((cell (cons init-value "any value")))
      (lambda msg
	(case (1st msg)
	  ((type) "box")
	  ((show) (car cell))
	  ((update!) (for-effect-only
		      (set-car! cell (2nd msg))))
	  ((swap!) (let ((ans (car cell)))
		     (set-car! cell (2nd msg))
		     ans))
	  ((reset!) (for-effect-only
		     (set-car! cell init-value)))
	  (else (delegate base-object msg)))))
    ))

;; counter
(define counter-maker
  (lambda (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
	(case (1st msg)
	  ((type) "counter")
	  ((update!) (let ((result (unary-proc
				    (send total 'show))))
		       (send total 'update! result)))
	  ((swap!) (delegate base-object msg))
	  (else (delegate total msg)))))
    ))

(define counter-maker2
  (lambda (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
	(case (1st msg)
	  ((type) "counter")
	  ((update!) (send total 'update!
			   (unary-proc (send total 'show))))
	  ((show reset) (delegate total msg))
	  (else (delegate base-object msg)))))
    ))

;; accumulator
(define accumulator-maker
  (lambda (init-value binary-proc)
    (let ((total (box-maker init-value)))
      (lambda msg
	(case (1st msg)
	  ((type) "accumulator")
	  ((update!)
	   (send total 'update!
		 (binary-proc (send total 'show) (2nd msg))))
	  ((swap!) (delegate base-object msg))
	  (else (delegate total msg)))))
    ))

(define acc (accumulator-maker 100 -))

;; gauge
(define gauge-maker
  (lambda (init-value unary-proc-up unary-proc-down)
    (let ((total (box-maker init-value)))
      (lambda msg
	(writeln msg)
	(newline)
	(writeln (1st msg))
	(newline)
	(case (1st msg)
	  ((type) "gauge")
	  ((up!) (send total 'update!
		       (unary-proc-up (send total 'show))))
	  ((down!) (send total 'update!
			 (unary-proc-down (send total 'show))))
	  ((swap! update!) (delegate base-object msg))
	  (else (delegate total msg)))))
    ))


(define g (gauge-maker 10 add1 sub1))

;; return an object, which take msg as arguments
;; it has a property as state storage
(define accumulator-maker2
  (lambda (init-value)
    (let ((total (box-maker init-value))
	   (proc (lambda (x,oldx)
		   (if (> x oldx)
		       (send total 'update! x)
		       (send total 'show))))
	   )
      (lambda msg
	(writeln msg)
	(newline)
	(writeln (1st msg))
	(newline)
	(case (1st msg)
	  ((type)
	   (writeln (1st msg))
	   (newline)
	   ;;(send total 'type)
	   )
	  ;; ((update!) (send total 'update!
	  ;; 		   (proc (2nd msg) (send total 'show))))
	  ;;(else (writeln "over")))
	;; (case (1st msg)
	;;   ((type) "accumulator-maker2")
	;;   ;; ((update!) (proc (2nd msg) (send total 'show)))
	  (else (delegate total msg))))
      )))

(define accumulator-maker3
  (lambda (init-value)
    (let ((total (box-maker init-value)))
      (lambda msg
	;;(writeln msg) (newline)
	(case (1st msg)
	  ((type) (writeln (1st msg)))
	  ((update!) (if (> (2nd msg) (send total 'show))
			 (send total 'update! (2nd msg))
			 (send total 'show)))
	  (else (delegate total msg))
	  )
	))))

(define double-box-maker
  (lambda (init-value1 init-value2)
    (let ((box1 (box-maker init-value1))
	  (box2 (box-maker init-value2)))
      (lambda msg
	(case (1st msg)
	  ((type) "double-box")
	  ((show-left) (send box1 'show))
	  ((show-right) (send box2 'show))
	  ((update-left!) (send box1 'update! (2nd msg)))
	  ((update-right!) (send box2 'update! (2nd msg)))
	  ((reset!)   (send box1 'update! init-value1)
	              (send box2 'update! init-value2))
	  (else (delegate base-object msg)))))
    ))
	   
;; stacks, LIFO
(define stack-maker
  (lambda ()
    (let ((stk '()))
      (lambda msg
	(case (1st msg)
	  ((type) "stack")
	  ((empty?) (null? stk))
	  ((push!) (set! stk (cons (2nd msg) stk)))
	  ((top) (if (null? stk)
		     (error "top: The stack is empty.")
		     (car stk)))
	  ((pop!) (if (null? stk)
		      (error "pop!: The stack is empty.")
		      (set! stk (cdr stk))))
	  ((size) (length stk))
	  ((print) (display "TOP: ")
	   (for-each (lambda (x)
		       (display x)
		       (display " "))
		     stk))
	  (else (delegate base-object msg)))))
    ))

(define grouping-maker
  (lambda (ls)
    (let ((stack (stack-maker)))
      (letrec
	  ((helper
	    (lambda (ils)
	      ;; (writeln ils)
	      ;; (newline)
	      (if (null? ils)
		  (send stack 'print)
		  (begin
		    (case (car ils)
		      (("{") (send stack 'push! (car ils)))
		      (("}" ")" "]") (send stack 'top)
		       (send stack 'pop!))
		      (else (writeln (car ils))
			    (newline)))
		    (helper (cdr ils))
		  ))
	      )))
	(helper ls)))))
      
;; (define grouping-test
;;   (lambda (ls)
;;     (if (null? ls)
;; 	(writeln 'end)
;; 	(begin
;; 	  (case (car ls)
;; 	    ((3) (writeln 'met))
;; 	    (else (writeln (car ls))
;; 		  (newline)
;; 		  (grouping-test (cdr ls))))
;; 	  ))))

;; queue

(define circular-list-maker
  (lambda ()
    (let ((marker '())
	  (size-gauge (gauge-maker 0 add1 sub1)))
      (lambda msg
	(case (1st msg)
	  ((type) "circular list")
	  ((empty?) (null? maker))
	  ((insert!) (send size-gauge 'up!)
	   (if (null? marker)
	       (begin
		 (set! marker (cons (2nd msg) '()))
		 (set-car! marker marker))
	       (set-cdr! maker
			 (cons (2nd msg) (cdr marker)))))
	  ((head) (if (null? marker)
		      (error "head: The list is empty.")
		      (car (cdr marker))))
	  ((delete!) (if (null? marker)
			 (error "delete!: The circular list is empty")
			 (begin
			   (send size-gauge 'down!)
			   (if (eq? marker (cdr marker))
			       (set! marker '())
			       (set-cdr! marker (cdr (cdr marker)))))))
	  ((move!) (if (null? marker)
		       (error "move!: The circular list is empty")
		       (set! marker (cdr marker))))
	  ((size) (send size-gauge 'show))
	  ((print) (if (not (null? marker))
		       (let ((next (cdr marker)))
			 (set-cdr! marker '())
			 (for-each
			  (lambda (x) (display x)
				  (display " "))
			  next)
			 (set-cdr! marker next)))
	   (newline))
	  (else (delegate base-object msg)))))))

(define stack-maker
  (lambda ()
    (let ((c (circular-list-maker)))
      (lambda msg
	(case (1st msg)
	  ((type) "stack")
	  ((push!) (send c 'insert! (2nd msg)))
	  ((pop!) (send c 'delete!))
	  ((top) (send c 'head))
	  ((print) (display "TOP: ") (send c 'print))
	  ((insert! head delete! move!)
	   (delegate base-object msg))
	  (else (delegate c msg)))))))

(define queue-maker
  (lambda ()
    (let ((c (circular-list-maker)))
      (lambda msg
	(case (1st msg)
	  ((type) "queue")
	  ((enqueue!) (send c 'insert! (2nd msg))
	   (send c 'move!))
	  ((dequeue!) (send c 'delete!))
	  ((front) (send c 'head))
	  ((print) (display "FRONT: ") (send c 'print))
	  ((insert! head delete! move!)
	   (delegate base-object msg))
	  (else (delegate c msg)))))))

(define bucket-maker
  (lambda ()
    (let ((table '()))
      (lambda msg
	(case (1st msg)
	  ((type) "bucket")
	  ((lookup)
	   (let ((key (2nd msg)) ((succ 3rd msg))
		 (fail (4th msg)))
	     (lookup key table (lambda (pr)
				 (succ (cdr pr)))
		     fail)))
	  ((update!)
	   (let ((key (2nd msg))
		 (updater (3rd msg))
		 (initializer (4th msg)))
	     (lookup key table
		     (lambda (pr)
		       (set-cdr! pr (updater (cdr pr))))
		     (lambda ()
		       (let ((pr (cons key (initializer key))))
			 (set! table (cons pr table)))))))
	  (else (delegate base-object msg)))))))


				    

;; test, p419
(4th '(1 2 3 4 5 6))
(define f (lambda (x) (+ x 10)))
(f 40)
(f 5)
(set! f (lambda (x) (* x 10)))
(f 5)

(fib-m 6)
(fib-m 6)
(fib-m 10)
(fib-m 20)
 
(counter-maker 0 (lambda (x) (+ 5 x)))
(send g 'show)
(send g 'type)
(send g 'up!)
(send g 'down!)
;; p422
(send g 'show)

;; (define acc-max2
;;   (accumulator-maker2 0))

;;(acc-max 'type)
;;(send acc-max2 'type)
;;(send acc-max2 'update! 3)
;;(send acc-max2 'show)

(define acc-max3
  (accumulator-maker3 0))

(send acc-max3 'type)
(send acc-max3 'show)
(send acc-max3 'update! 3)
(send acc-max3 'show)
(send acc-max3 'update! 7)
(send acc-max3 'update! 2)
(send acc-max3 'update! 4)
(send acc-max3 'update! 10)
(send acc-max3 'update! 1)
(send acc-max3 'update! 5)
(send acc-max3 'show)

(define dbox (double-box-maker 1 1))
(send dbox 'show-left)
(send dbox 'update-left! 99)
(send dbox 'update-right! 98)
;;(send dbox 'reset!)
(send dbox 'show-left)
(send dbox 'show-right)

(define r (stack-maker))
(define s (stack-maker))

(send s 'print)
(send s 'push! 'a)
(send s 'print)

(define grouping '(13 + 5 * "{" "[" 14 - 3 * "(" 12 - 7 ")" "]" - 15 "}"))
(writeln grouping)
(grouping-maker grouping)
(writeln "{")

(equal? "{" "{")

;; (grouping-test grouping)
(define testlist '( 1 2 3 4 5 6 7 8 9 10))
(3rd testlist)
(4th testlist)

(define b (bucket-maker))
(send b 'lookup 'a (lambda (x) x) (lambda () 'no))
  
