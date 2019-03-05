;;
(load "util.scm")

(define map
  (lambda (proc ls)
    (if (null? ls)
	'()
	(cons (proc (car ls)) (map proc (cdr ls))))
    ))

(define for-each2
  (lambda (proc ls)
    (if (not (null? ls))
	(begin
	  (proc (car ls))
	  (for-each2 proc (cdr ls))))
    ))

;; (define reduce
;;   (lambda (proc ls)
;;     (if (null? ls)
;; 	'()
;; 	(proc (car ls) (reduce proc (cdr ls))))
;;     ))

(define add2
  (letrec ((list-add
	    (lambda (ls)
	      (display ls)
	      (if (null? ls)
	       	  0
		  ;;(+ 1 (list-add (cdr ls)))))))
	      	  (+ (car ls) (list-add (cdr ls)))))))
	      ;; (write ls))))
    (lambda args
      (list-add args))))

(define list2
  (lambda args
    args))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))
    ))

(define h (compose sqrt add1))

(define compose-many
  (lambda args
    (lambda (x)
      (letrec ((compose2
		(lambda (ls y)
		  (if (null? ls)
		      y
		      (compose2 (cdr ls) ((car ls) y))))))
	(compose2 args x)))))
		   
(define subtract
  (lambda (x y)
    (- x y)))
      
(define map-first-two
  (lambda (proc ls)
    (letrec ((map-helper
	      (lambda (n ls)
		(cond
		 ((null? n) '())
		 ((null? (cdr ls)) (list (proc n (car ls))))
		 (else
		  (cons (proc n (car ls))
			(map-helper (car ls) (cdr ls))
			))))))
      (cond
       ((null? ls) '())
       ((= (length ls) 1) ls)
       (else
	(map-helper (car ls) (cdr ls))))
    )))

(define reduce
  (lambda (proc ls)
    (letrec ((reduce-helper
	      (lambda (n ls)
		(cond
		 ((null? (cdr ls)) (proc n (car ls)))
		 (else
		  (reduce-helper (proc n (car ls)) (cdr ls)))))))
      (reduce-helper (car ls) (cdr ls)))
    ))

(define andmap
  (lambda (pred ls)
    (reduce (lambda (x y) (and x y)) (map pred ls))))

(define map2
  (lambda (pred ls1 ls2)
    (cond
     ((null? ls1) '())
     (else
      (cons (pred (car ls1) (car ls2))
	    (map2 pred  (cdr ls1) (cdr ls2)))))
    ))

(define curried+
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define apply-to-all
  (lambda (proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		'()
		(cons (proc (car ls))
		      (helper (cdr ls)))))))
      helper)))

(define is-divisible-by?
  (lambda (n)
    (lambda (k)
      (zero? (remainder n k)))
    ))

(define flat-recur
  (lambda (seed list-proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(list-proc (car ls) (helper (cdr ls)))))))
      helper)))

(define mult-by-scalar
  (lambda (n)
    (flat-recur '() (lambda (x y)
		      (cons (* n x) y)))))

(define filter-out
  (lambda (item)
    (flat-recur '()
		(lambda (x y)
		  (if (equal? item x)
		      y
		      (cons x y))))
    ))

;; (define insert-left
;;   (lambda (num)
;;     (flat-recur '()
;; 		(lambda (x y)
;; 		  (if (item
(define listof
  (lambda (m n)
    (if (> m n)
	'()
	(cons m (listof (add1 m) n)))))

(define partial-sum
  (lambda (pred start end)
    (let* ((ls (listof start end)))
      ((flat-recur 0
		   (lambda (x y)
		     (+ (pred x) y)))
       ls))))

(define partial-product
  (lambda (pred start end)
    (let* ((ls (listof start end)))
      ((flat-recur 1
		   (lambda (x y)
		     (* (pred x) y)))
       ls))
    ))
      
(define partial
  (lambda (x operator)
    (lambda (pred start end)
      (let* ((ls (listof start end)))
	((flat-recur x
		     (lambda (x y)
		       (operator (pred x) y)))
	 ls)))
    ))

(define partial-sum2 (partial 0 +))
(define partial-product2 (partial 1 *))

(define filter-in-all-c
  (lambda (pred)
      (letrec
	  ((filter-helper
	    (lambda (ls)
	      (if (null? ls)
		  '()
		  (let ((a (car ls)))
		    (if (or (pair? a) (null? a))
			(cons (filter-helper a)
			      (filter-helper (cdr ls)))
			(if (pred a)
			    (cons a (filter-helper (cdr ls)))
			    (filter-helper (cdr ls)))))))))
	filter-helper)))

(define sum-all
  (lambda (ls)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		0
		(let ((a (car ls)))
		  (if (or (pair? a) (null? a))
		      (+ (helper a) (helper (cdr ls)))
		      (+ a (helper (cdr ls)))))))))
      (helper ls))))
      
(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(let ((a (car ls)))
		  (if (or (pair? a) (null? a))
		      (list-proc (helper a)
				 (helper (cdr ls)))
		      (item-proc a
				 (helper (cdr ls)))))))))
      helper)))
(define sum-all2 (deep-recur 0 + +))
(define remove-all-c
  (lambda (item)
    (deep-recur '()
		(lambda (x y)
		  (if (= item x)
		      y
		      (cons x y)))
		cons)))

(define product-all
  (deep-recur 1 * *))

(define reverse-all
  (deep-recur '()
	      (lambda (x y)
		(cons x y))
	      (lambda (x y)
		(cons  x y)))))
(define reverse-all2
  (deep-recur '()
	      (lambda (x y)
		(append y (list x)))
	      (lambda (x y)
		(append y (list x)))))
		
		  
   


;; some tests
(map add1 '(1 3 5 7 9))
(map (lambda (num) (+ num 2)) '(1 3 5 7 9))
(for-each2 display '("Hello." " " "How are you?"))
;; (add 1 3 5)
(reduce add1 '(1 2 3 4))
(add2 1 2 3 4 5)

(list2 'a 'b 'c)
(apply add2 '( 2 4))
(h 3)
(h 8)
((compose-many add1 add1 add1 add1) 1)
(subtract 3 2)

(let ((h (lambda (x) (cons x x))))
  (map h '((1 2) (3 4) (5 6))))

(map-first-two + '(2 3 4 5 7))
(map-first-two max '(2 4 3 5 4 1))

(display 'reduce)
(reduce + '(1 3 5 7 9))
(reduce max '(2 -4 6 8 3 1))
(reduce (lambda (x y) (and x y)) '(#t #t #t #t))
(andmap positive? '(3 4 6 9))

(map2 + '(1 2 3 4) '(5 7 9 11))
(map2 (let ((n 5))
	(lambda (x y)
	  (and (< x n) (< n y))))
      '(1 3 2 1 7)
      '(9 11 4 7 8))

((curried+ 5) 3)
((mult-by-scalar 3) '(1 3 5))
((mult-by-scalar 5) '())
((filter-out 4) '( 1 2 3 4 5 6 7 8 9 0))

(partial-sum (lambda (m) (* m m)) 3 4)
(listof 3 4)
(partial-sum (lambda (m) (* m m)) 3 7)
(partial-product (lambda (m) (* m m)) 3 7)
(partial-sum2 (lambda (m) (* m m)) 3 7)
(partial-product2 (lambda (m) (* m m)) 3 7)

((filter-in-all-c even?) '((4 5) 2 (3 5 (8 7))))
((filter-in-all-c odd?) '((4 5) 2 (3 5 (8 7))))
(sum-all '(3 (1 4) (2 (-3 5))))
(sum-all2 '(3 (1 4) (2 ( -3 5))))
((remove-all-c 4) '(1 2 3 4 5 6))
(product-all '(3 (1 4) (2 2)))
(reverse-all '(3 (1 4) (2 (-3 5))))
(reverse-all2 '(3 (1 4) (2 (-3 5))))
