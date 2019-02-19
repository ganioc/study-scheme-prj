;; 
;; tea

(define NUM 10)
(define NUM2 NUM)
(define EXAM_CONS '(1 2 3 4 5 6 7 8))
(define type-of
  (lambda (item)
    (if (pair? item)
	'pair
	(if (null? item)
	    'empty-list
	    (if (number? item)
		'number
		(if (symbol? item)
		    'symbol
		    'some-other-type))))))

(define last-item
  (lambda (ls)
    (cond
     ((null? (cdr ls)) (car ls))
     (else (last-item (cdr ls))))))

(define remove-lst
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((eqv? item (car ls)) (cdr ls))
     (else (cons (car ls) (remove-lst item (cdr ls)))))
    )
  )

(define writeln
  (lambda args
    (for-each display args)
    (newline)))
;; 
(define remove-1st-trace
  (lambda (item ls)
    (cond
     ((entering (null? ls) ls 1)
      (leaving '() 1))
     ((entering (equal? (car ls) item) ls 2)
      (leaving (cdr ls) 2))
     ((entering 'else ls 3)
      (leaving
       (cons (car ls) (remove-1st-trace item (cdr ls)))
       3)))))

(define entering
  (lambda (test input cond-clause-number)
    (begin
      ;; (writeln "Entering" input)
      (if test (writeln "Entering cond-clause-"
			cond-clause-number " with ls = " input))
      test)))

(define leaving
  (lambda (result cond-clause-number)
    (begin
      (writeln "Leaving cond-clause-"
	       cond-clause-number " with result = " result)
      result)))

(define swapper
  (lambda ( x y ls)
    (cond
     ((null? ls) ls)
     ((eqv? x (car ls)) (cons y (swapper x y (cdr ls))))
     ((eqv? y (car ls)) (cons x (swapper x y (cdr ls))))
     (else (cons (car ls) (swapper x y (cdr ls))))
     )
    )
  )

(define swapper1
  (lambda (x y ls)
    (cond
     ((null? ls) '())
     (else (cons (swap-tester x y (car ls))
		 (swapper1 x y (cdr ls)))
	   ))))

;; Use a sub-fucntion
(define swap-tester
  (lambda ( x y a)
    (cond
     ((equal? x a) y)
     ((equal? y a) x)
     (else a))
    )
  )


NUM

;; (display 'NUM2)
NUM2
(car (cdr EXAM_CONS))
(cddr EXAM_CONS)

(pair? (cons 1 '()))

(type-of EXAM_CONS)
(eq? 'ab 'Ab)

(last-item EXAM_CONS)
(remove-lst 5 EXAM_CONS)
(display 'Wumi)

(begin
  (+ 3 5)
  (- 5 11))

(writeln (display 'Hunluan) 121)

(remove-1st-trace 'c '(a b c d))

(remove-1st-trace 'e '(a b c d))

(remove-1st-trace 'a '(a b c d))

(remove-1st-trace 'a '(a))

(remove-1st-trace 'a '())

(remove-1st-trace 'a '( a b a))

(swapper 'cat 'dog '(my cat eats dog food))
(swapper 'john 'mary '(john loves mary))
(swapper 'a 'b '(c (a b) d))
(swapper 'a 'b '())

(swapper1 'cat 'dog '(my cat eats dog food))
''()
