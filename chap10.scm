;; chap10.scm
(load "util.scm")

(define insert
  (lambda (a ls)
    (cond
     ((null? ls) (cons a '()))
     ((< a (car ls)) (cons a ls))
     (else (cons (car ls) (insert a (cdr ls)))))
    ))

(define insertsort
  (lambda (ls)
    (if (singleton-list? ls)
	ls
	(insert (car ls) (insertsort (cdr ls))))
    ))

(define vector-insert!
  (lambda (k vec)
    (let ((val (vector-ref vec k)))
      (letrec
	  ((insert-h
	    (lambda (m)
	      (if (zero? m)
		  (vector-set! vec 0 val)
		  (let ((comp (vector-ref vec (sub1 m))))
		    (if (< val comp)
			(begin
			  (vector-set! vec m comp)
			  (insert-h (sub1 m)))
			(vector-set! vec m val)))))))
	(insert-h k)))
    ))
		

(define vector-insertsort!
  (lambda (v)
    (let ((size (vector-length v)))
      (letrec
	  ((sortloop
	    (lambda (k)
	      (if (< k size)
		  (begin
		    (vector-insert! k v)
		    (sortloop (add1 k)))))))
	(sortloop 1)))
    ))

(define vector-insertsort
  (lambda (vec)
    (let ((v (vector-copy vec)))
      (vector-insertsort! v)
      v)
    ))

(define make-groups
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((null? (cdr ls)) (list ls))
     (else (let ((a (car ls))
		 (gps (make-groups (cdr ls))))
	     (if (< (cadr ls) a)
		 (cons (list a) gps)
		 (cons (cons a (car gps)) (cdr gps))))))
    ))

(define pair-merge
  (lambda (sublists)
    (cond
     ((null? sublists) '())
     ((null? (cdr sublists)) sublists)
     (else
      (cons (merge (car sublists) (cadr sublists))
	    (pair-merge (cddr sublists)))))
    ))

(define nat-mergesort
  (lambda (ls)
    (if (null? ls)
	'()
	(letrec ((sort
		  (lambda (gps)
		    (if (null? (cdr gps))
			(car gps)
			(sort (pair-merge gps))))))
	  (sort (make-groups ls))))
    ))
;; left -> top-left
;; right -> top-right
;; merge these 2 groups into newvec
;; 
(define vector-merge!
  (lambda (newvec vec)
    (lambda (left top-left right top-right)
      (letrec
	  ((mergeloop
	    (lambda (left right i)
	      (cond
	       ((and (< left top-left) (< right top-right))
		(if (< (vector-ref vec left) (vector-ref vec right))
		    (begin
		      (vector-set! newvec i (vector-ref vec left))
		      (mergeloop (add1 left) right (add1 i)))
		    (begin
		      (vector-set! newvec i (vector-ref vec right))
		      (mergeloop left (add1 right) (add1 i)))))
	       ((< left top-left)
		(vector-set! newvec i (vector-ref vec left))
		(mergeloop (add1 left) right (add1 i)))
	       ((< right top-right)
		(vector-set! newvec i (vector-ref vec right))
		(mergeloop left (add1 right) (add1 i)))))))
	(mergeloop left right left)))
    ))

;; copy from vec2 to vec1, j~k 
(define vector-change!
  (lambda (vec1 j k vec2)
    (letrec ((loop (lambda (i)
		     (if (<= i k)
			 (begin
			   (vector-set! vec1 i
					(vector-ref vec2 i))
			   (loop (add1 i)))))))
      (loop j))))


(define vector-mergesort!
  (lambda (vec1)
    (let ((vec-size (vector-length vec1)))
      (let ((adjust (lambda (k) (min k vec-size)))
	    (vec2 (make-vector vec-size))
	    (max-index (sub1 vec-size)))
	(letrec
	    ((merge-pass
	      (lambda (group-size count)
		(if (> group-size max-index)
		    (if (even? count)
			(vector-change! vec1 0 max-index vec2))
		    (let ((newvec
			   (if  (odd? count) vec2 vec1))
			  (vec (if (odd? count) vec1 vec2)))
		      (let ((merge! (vector-merge! newvec vec)))
			(letrec
			    ((group-ends
			      (lambda (left top-left right top-right)
				(if (<= left max-index)
				    (begin
				      (merge! left top-left
					      right top-right)
				      (let
					  ((new-right
					    (+ top-right group-size)))
					(group-ends
					 top-right
					 (adjust new-right)
					 new-right
					 (adjust (+ new-right
						    group-size)))))))))
			  (group-ends 0
				      (adjust group-size)
				      group-size
				      (adjust (* 2 group-size)))))
		      (merge-pass (* group-size 2) (add1 count)))))))
	  (merge-pass 1 1))))))

(define vector-swap!
  (lambda (vec i j)
    (let ((temp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j temp))))

(define partition
  (lambda (v low high)
    (let ((pivot (vector-ref v low)))
      (letrec
	  ((search
	    (lambda (left right)
	      (letrec
		  ((search-up
		    (lambda (i)
		      (cond
		       ((= i (add1 right)) (sub1 i))
		       ((> (vector-ref v i) pivot) i)
		       (else (search-up (add1 i))))))
		   (search-down
		    (lambda (i)
		      (cond
		       ((or (= i (sub1 left))
			    (< (vector-ref v i) pivot)) i)
		       (else
			(search-down (sub1 i)))))))
		(let ((new-left (search-up left))
		      (new-right (search-down right)))
		  (if (< new-left new-right)
		      (begin
			(vector-swap! v new-left new-right)
			(search (add1 new-left) (sub1 new-right)))
		      (begin
			(vector-swap! v low new-right)
			new-right)))))))
	(search (add1 low) high)))))


(define quicksort
  (letrec
      ((collect
	(lambda (pivot ls lgroup rgroup)
	  (if (null? ls)
	      (append (quicksort lgroup)
		      (cons pivot (quicksort rgroup)))
	      (if (< (car ls) pivot)
		  (collect pivot (cdr ls)
			   (cons (car ls) lgroup)
			   rgroup)
		  (collect pivot (cdr ls)
			   lgroup
			   (cons (car ls) rgroup)))))))
    (lambda (ls)
      (if (or (null? ls) (null? (cdr ls)))
	  ls
	  (collect (car ls) (cdr ls) '() '())))))

(define vector-quicksort!
  (lambda (v)
    (letrec
	((qsort (lambda (low high)
		  (if (< low high)
		      (let ((middle (partition v low high)))
			(qsort low (sub1 middle))
			(qsort (add1 middle) high))))))
      (qsort 0 (sub1 (vector-length v))))
    ))

(define table10-17
  '(("Smith, Harold W." 2324 43 1974 "Fox,Charles Q." 49325)
    ("Jones, Mary Ann"  1888 54 1965 "none"           65230)
    ("White, Thomas P." 3403 34 1982 "Smith, Harold"  27300)))

(define random-list
  (lambda (n)
    (letrec ((build-list
	      (lambda (k)
		(if (zero? k)
		    '()
		    (cons (random n)
			  (build-list (sub1 k)))))))
      (build-list n))))

(define timer
  (lambda (proc arg)
    (let* ((start (get-universal-time))
	   (val (proc arg))
	   (finish (get-universal-time))
	   (elapsed-time (/ (- finish start) 100)))
      (writeln "Time = " elapsed-time ", Answer = " val))
    ))

(define binary-search
  (lambda (rel)
    (lambda (vec target)
      (letrec
	  ((search
	    (lambda (left right)
	      (if (< right left)
		  (writeln " The search failed")
		  (let ((middle (floor (/ (+ left right) 2))))
		    (let ((mid-val (vector-ref vec middle)))
		      (cond
		       ((rel target mid-val)
			(search left (sub1 middle)))
		       ((rel mid-val target)
			(search (add1 middle) right))
		       (else middle))))))))
	(search 0 (sub1 (vector-length vec)))))
    ))

(define list-linear-search
  (lambda (ls num)
    (let ((length (vector-length ls)))
      (letrec ((search
		(lambda (i)
		  (if (>= i length)
		      (writeln "Fail to find")
		      (if (= (vector-ref ls i) num)
			  (writeln i)
			  (search (add1 i)))))))
	(search 0)))
    ))

(define unlist
  (lambda (proc)
    (lambda (ls)
      (apply proc ls))
    ))

(define set10-17 (list->set table10-17))  
;; p361 mark					  
(define age-test?
  (unlist
   (lambda (name id age yr-emp supervisor salary)
     (> age 25))))

;;(define for-all
;; (define all-do2
;;   (lambda (ls)
;;     (letrec
;; 	((helper
;; 	  (lambda (ls1)
;; 	    (if (not (null? ls1))
;; 		(begin
;; 		  ((unlist (lambda (name id age yr-emp supervisor salary))
;; 			   (writeln name)
;; 			   (newline)) (cdr ls1))
;; 		  (helper (cdr ls1))))))))
;;       (helper ls)))

(define exam3
  (lambda args
    (begin
      (writeln 'Thisisexam3)
      (newline))))
(define do-all
  (lambda (ls)
    (letrec ((time 2)
	     (proc
	      (lambda (name id age yr-emp supervisor salary)
		;; (writeln name)
		(> age 45)
		;;(newline)))
		))
	     (helper
	      (lambda (ils)
		(if (null? ils)
		    (writeln 'end)
		    (begin
		      ((unlist proc) (car ils))
		      (helper (cdr ils)))))))
      (writeln time)
      (newline)
      (helper ls))
    ))
;; test
(insertsort '(50 40 30 20 10))
(insert 50 (insertsort '(40 30 20 10)))
(make-groups '(1 3 5 4 2))
(merge '(1 2) '(9 10))
;; p339
(define vec01 #(2  4  8  10 9  5  1 77))
(define vec02 #(12 13 14 5  6  7 11 1))

vec01
vec02

((vector-merge! vec01 vec02) 0 3 3 6)
vec01
vec02

(quicksort '(4 5 6 3))

(define rand100 (random-list 100))
rand100

(define v-rand100 (list->vector rand100))
v-rand100

(get-universal-time)

(timer quicksort '(4 5 6 7 2 3 1))

(string<? "a" "c")
(string<? "cba" "abc")

(define alpha-search (binary-search string<?))
(let ((names (vector "Ann S" "Ben J" "Ed A" "Guy S" "Kay W")))
  (alpha-search names "Ed A"))

(list-linear-search (vector 1 2 4 5 64 67 69 69) 69)

((unlist +) '(2 3))
table10-17
(list->set table10-17)

;; (define set10-17 (list->set table10-17))
;; ((for-all age-test?) set10-17)
((unlist (lambda (name id age yr-emp supervisor salary)
	    (writeln name)
	    (newline))) '(1 2 3 4 5 6))

;;(all-do2 table10-17)
(exam3)
(do-all '())
(do-all table10-17)





