;;
(load "util.scm")

(define LST1
  '(a b c))
(define LST2
  '(c d))

(define append_ex
  (lambda (lst1 lst2)
    (if (null? lst1)
	lst2
	(cons (car lst1) (append_ex (cdr lst1) lst2)))
    ))

(define reverse_ex
  (lambda (lst)
    (if (null? lst)
	'()
	(append_ex (reverse_ex (cdr lst)) (list (car lst))))
    ))

(define merge
  (lambda (sorted-ntpl1 sorted-ntpl2)
    (cond
     ((null? sorted-ntpl1) sorted-ntpl2)
     ((null? sorted-ntpl2) sorted-ntpl1)
     ((< (car sorted-ntpl1) (car sorted-ntpl2))
      (cons (car sorted-ntpl1)
	    (merge (cdr sorted-ntpl1) sorted-ntpl2)))
     (else (cons (car sorted-ntpl2)
		 (merge sorted-ntpl1 (cdr sorted-ntpl2)))))
    ))

(define remove
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (remove item (cdr ls)))
     (else (cons (car ls) (remove item (cdr ls)))))
    ))

(define insert-left
  (lambda (new old lst)
    (cond
     ((null? lst) '())
     ((equal? old (car lst)) (cons new (cons old (insert-left new old (cdr lst)))))
     (else (cons (car lst) (insert-left new old (cdr lst)))))
    ))
     
(define insert-right
  (lambda (new old lst)
    (cond
     ((null? lst) '())
     ((equal? old (car lst)) (cons old (cons new (insert-right new old (cdr lst)))))
     (else (cons (car lst) (insert-right new old (cdr lst)))))
    ))

(define subst
  (lambda (new old lst)
    (cond
     ((null? lst) '())
     ((equal? old (car lst)) (cons new (subst new old (cdr lst))))
     (else (cons (car lst) (subst new old (cdr lst)))))
    ))

(define deepen-1
  (lambda (lst)
    (cond
     ((null? lst) '())
     (else (cons (cons (car lst) '()) (deepen-1 (cdr lst))))
     )
    ))

(define count-all-lst
  (lambda (lst)
    (cond
     ((pair? lst) (+ (count-all-lst (car lst))
		     (count-all-lst (cdr lst))))
     (else 0))
    ))

(define count-all-check
  (lambda (lst)
    ;; (bkpt 'count-all-check)
    (cond
     ((pair? lst) (+ (count-all-check (car lst))
		     (if (null? (cdr lst))
			 0
			 (count-all-check (cdr lst)))))
     (else 1))
    ))
    
(define count-all
  (lambda (lst)
    (cond
     ((equal? lst '()) 0)
     (else (count-all-check lst)))
    ))

(define remove-all
  (lambda (item lst)
    (cond
     ((null? lst) '())
     ((pair? (car lst))
      (cons (remove-all item (car lst)) (remove-all item (cdr lst))))
     (else
      (if (equal? item (car lst))
	  (remove-all item  (cdr lst))
	  (cons (car lst) (remove-all item (cdr lst))))))
    ))

(define reverse-all
  (lambda (lst)
    (cond
     ((null? lst) '())
     ((pair? (car lst))
      (append (reverse-all (cdr lst)) (list (reverse-all (car lst)))))
     (else
      (append (reverse-all (cdr lst)) (list (car lst)))))
    ))


(define subst-all
  (lambda (new old lst)
    (cond
     ((null? lst) '())
     ((pair? (car lst))
      (cons (subst-all new old (car lst)) (subst-all new old (cdr lst))))
     (else
      (if (equal? old (car lst))
	  (cons new (subst-all new old  (cdr lst)))
	  (cons (car lst) (subst-all new old (cdr lst))))))
    ))
      
(define insert-left-all
  (lambda (new old lst)
    (cond
     ((null? lst) '())
     ((pair? (car lst))
      (cons (insert-left-all new old (car lst)) (insert-left-all new old (cdr lst))))
     (else
      (if (equal? old (car lst))
	  (cons new (cons old (insert-left-all new old (cdr lst))))
	  (cons (car lst) (insert-left-all new old (cdr lst))))))
    ))

(define sum-all
  (lambda (lst)
    (cond
     ((null? lst) 0)
     ((pair? (car lst))
      (+ (sum-all (car lst)) (sum-all (cdr lst))))
     (else
      (+ (car lst) (sum-all (cdr lst)))))
    ))

;; tree operations
(define depth
  (lambda (item)
    (if (not (pair? item))
	0
	(max (add1 (depth (car item))) (depth (cdr item))))
    ))

(define flatten
  (lambda (lst)
    (cond
     ((null? lst) '())
     ((pair? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
     (else
      (cons (car lst) (flatten (cdr lst)))))
    ))

(define member-all?
  (lambda (item ls)
    (if (null? ls)
	#f
	(or (equal? (car ls) item)
	    (and (not (pair? (car ls)))
		 (member-all? item (cdr ls)))
	    (and (pair? (car ls))
		 (or (member-all? item (car ls))
		     (member-all? item (cdr ls))))))
    ))

(define remove-leftmost
  (lambda (item lst)
    (cond
     ((null? lst) '())
     ((equal? (car lst) item) (cdr lst))
     ((not (pair? (car lst )))
      (cons (car lst) (remove-leftmost item (cdr lst))))
     ((member-all? item (car lst))
      (cons (remove-leftmost item (car lst)) (cdr lst)))
     (else (cons (car lst) (remove-leftmost item (cdr lst)))))
    ))

(define count-parens-all
  (lambda (lst)
    (cond
     ((null? lst) 2)
     ((pair? lst) (+ (count-parens-all (car lst)) (count-parens-all (cdr lst))))
     (else 0))
    ))

(define count-background-all
  (lambda (item lst)
    (cond
     ((null? lst) 0)
     ((pair? (car lst)) (+ (count-background-all item (car lst)) (count-background-all item (cdr lst))))
     (else (if (equal? item (car lst))
	       (count-background-all item  (cdr lst))
	       (+ 1 (count-background-all item (cdr lst))))))
    ))
	       
(define leftmost
  (lambda (lst)
    (cond
     ((null? lst) '())
     ((pair? (car lst)) (leftmost (car lst)))
     (else (car lst))
     )
    ))


(define rightmost
  (lambda (lst)
    (cond
     ((null? lst) '())
     ((pair? lst)
      (if (not (null? (cdr lst)))
	  (rightmost (cdr lst))
	  (rightmost (car lst))))
     (else lst))
    ))
;; For numbers

(define fact
  (lambda (n)
    (cond
     ((zero? n) 1)
     (else (* n (fact (sub1 n)))))
    ))

(define fact-it
  (lambda (n acc)
    (cond
     ((zero? n) acc)
     (else (fact-it (sub1 n) (* n acc))))
    ))

(define harmonic-sum-it
  (lambda (n)
    (cond
     ((zero? n) 0)
     (else (+ (/ 1 n) (harmonic-sum-it (sub1 n)))))
    ))

(define fib
  (lambda (n)
    (if (< n 2)
	n
	(+ (fib (- n 1)) (fib (- n 2))))
    ))

(define calls-fib
  (lambda (n)
    (if (< n 2)
	1
	(add1 (+ (calls-fib (sub1 n)) (calls-fib (- n 2)))))
    ))

(define adds-fib
  (lambda (n)
    (if (< n 2)
	0
	(add1 (+ (adds-fib (sub1 n)) (adds-fib (- n 2)))))
    ))

(define fib-it
  (lambda (n acc1 acc2)
    (if (= n 1)
	acc2
	(fib-it (sub1 n) acc2 (+ acc1 acc2)))))

(define fib2
  (lambda (n)
    (if (zero? n)
	0
	(fib-it n 0 1))
    ))

(define reverse-it
  (lambda (ls acc)
    (if (null? ls)
	acc
	(reverse-it (cdr ls) (cons (car ls) acc)))
    ))

(define reverse
  (lambda (ls)
    (reverse-it ls '())))

(define length-it
  (lambda (ls acc)
    ;;(writeln "ls=" ls "acc=" acc)
    ;;(newline)
    (if (null? ls)
	acc
	(length-it (cdr ls) (+ 1 acc)))))

(define length2
  (lambda (ls)
    (length-it ls 0)))

(define mk-asc-list-of-ints-it
  (lambda (n acc)
    (if (zero? n)
	acc
	(mk-asc-list-of-ints-it (sub1 n) (cons n acc)))))

(define mk-asc-list-of-ints
  (lambda (n)
    (mk-asc-list-of-ints-it n '())))

(define mk-desc-list-of-ints-it
  (lambda (n acc)
    (if (zero? n)
	acc
	(mk-desc-list-of-ints-it (sub1 n) (list acc n)))
    ))

(define mk-desc-list-of-ints
  (lambda (n)
    (mk-desc-list-of-ints-it n '())
    ))

(define mk-desc-list-of-ints2
  (lambda (n)
    (if (zero? n)
	'()
	(cons n (mk-desc-list-of-ints2 (sub1 n))))
    ))

(define occurs
  (lambda (item lst)
    (if (null? lst)
	0
	(if (pair? lst)
	    (+ (occurs item (car lst)) (occurs item (cdr lst)))
	    (if (equal? item lst)
		1
		0)))))

(define occurs-it
  (lambda (item lst acc)
    (if (null? lst)
	acc
	(if (pair? lst)
	    (+ (occurs-it item (car lst) acc) (occurs-it item (cdr lst) acc))
	    (if (equal? item lst)
		1
		0)))
    ))
	    

(define occurs2
  (lambda (item lst)
    (if (null? lst)
	0
	(occurs-it item lst 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below goes the test codes

;; (append LST1 LST2)
(append_ex LST1 LST2)
(reverse_ex LST1)
(reverse_ex LST2)
(reverse_ex '((1 2) (3 4) (5 6)))
(reverse_ex '(2 32 4 5))

(define tlst '(h i j))

;; It seems cons at the head will produce a linear pair
(cons 'a tlst)
(cons tlst 'a)

(insert-left 'z 'a '(a b a c a))
(insert-left 0 1 '(0  1 0 1))
(insert-left 'dog 'cat '(my dog is fun))
(insert-left 'two 'one '())

(insert-right 'z 'a '(a b a c a))
(insert-right 0 1 '(0 1 0 1))
(subst 'z 'a '(a b a c a))

(deepen-1 '(a b c d))
(deepen-1 '((a b) (c (d e)) f))
(count-all '(a b c))
(count-all '((a b) c () ((d (e)))))
(count-all '(() () ()))
(count-all '((())))
(count-all '())

(remove-all 'a '( a b c d))
(remove-all 'a '(a b c d e a))
(remove-all 'a '((a b (c a)) (b (a c) a)))

(reverse-all '(a (b c) (d (e f))))
(subst-all 'z 'a '(a (b (a c)) (a (d a))))
(subst-all 0 '(1) '(((1) (0))))
(subst-all 'one 'two '())
(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(b b))
(insert-left-all 'z 'a '())

(sum-all '((1 3) (5 7) (9 11)))
(sum-all '(1 (3 (5 (7 (9))))))
(sum-all '())

(remove-leftmost 'b '(a (b c) (c (b a))))
(remove-leftmost '(c d) '((a (b c)) ((c d) e)))
;; (car '())
(count-parens-all '())
(count-parens-all '((a b) c))
(count-parens-all '(((a () b) c) () ((d) e)))

(count-background-all 'a '((a) b (c a) d))
(count-background-all 'a '((((b (((a)) c))))))
(count-background-all 'b '())
(leftmost '((a b) (c (d e))))
(leftmost '((((c ((e f) g) h)))))
(leftmost '(() a))

(rightmost '((a b) (d (c d (f (g h) i) m n) u) v))
(rightmost '((((((b (c))))))))
(rightmost '(a ()))

(fact 3)
(add1 3)
(fact-it 3 1)
(fact-it 10 1)
(fact-it 50 1)
(fact-it 100 1)
;; (fact-it 3.5 1)
(exact->inexact (harmonic-sum-it 15))
(log 15)
(/ 1 5)
(sub1 5)

(fib 10)
(calls-fib 10)
(adds-fib 10)

(fib-it 6 0 1)

(length2 '(1 2 3 4))

(mk-asc-list-of-ints 10)
(mk-desc-list-of-ints 10)
(mk-desc-list-of-ints2 10)

(occurs 'a '(a b a c a d))
(occurs 'a '(b c a (b a) c a))
(occurs 'a '(b (c d)))

(occurs2 'a '(b c a (b a) c a))
(occurs2 'a '(a b a c a d))
