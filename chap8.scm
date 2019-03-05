;; chap8.scm
(load "util.scm")
(define both
  (lambda (pred)
    (lambda (arg1 arg2)
      (and (pred arg1) (pred arg2)))))

(define both2
  (lambda (arg1 arg2)
    (not ((at-least-one zero?) arg1 arg2))))

(define compose
  (lambda (pred1 pred2)
    (lambda (x)
      (pred1 (pred2 x)))
    ))

(define neither
  (lambda (pred)
    (lambda (arg1 arg2)
      (not (or (pred arg1) (pred arg2))))
    ))

(define neither2
  (lambda (arg1 arg2)
    ((both not) arg1 arg2))
  )

(define neither3
  (lambda (arg1 arg2)
    (not ((at-least-one one?) arg1 arg2))
    ))

(define at-least-one
  (lambda (pred)
    (lambda (arg1 arg2)
      (or (pred arg1) (pred arg2)))
    ))
(define at-least-one2
  (lambda (arg1 arg2)
    (not ((both zero?) arg1 arg2))
    ))

(define remove2
  (lambda (item ls)
    (letrec ((helper
	      (lambda (item ls)
		(cond
		 ((null? ls) '())
		 (else
		  (if (= item (car ls))
		      (cdr ls)
		      (cons (car ls)
			    (helper item (cdr ls)))))))))
      (helper item ls))
    ))
		  

;; set
;; The universe is defined upon 6 basic representation dependent terms,
;; tagged list
;; 

;; 0/
(define set-tag "set")

;; 1/
(define the-empty-set (cons set-tag '()))

;; 2/
(define empty-set?
  (lambda (s)
    (eq? s the-empty-set)))

;; 3/
(define set?
  (lambda (arg)
    (and (pair? arg) (eq? (car arg) set-tag))))

;; 4/
(define pick
  (lambda (s)
    (let ((ls (cdr s)))
      (if (null? ls)
	  (error "pick: The set is empty.")
	  (list-ref ls (random (length ls)))))
    ))

;; 5/
;; What is remove?
(define residue
  (lambda (elem)
    (lambda (s)
      (let ((ls (remove2 elem (cdr s))))
	(cond
	 ((null? ls) the-empty-set)
	 (else (cons set-tag ls)))))
    ))

;; 6/
(define adjoin
  (lambda (elem s)
    (cons set-tag (cons elem (cdr s)))
    ))



;; We use above 6 terms to define the whole universe

(define make-set
  (lambda args
    (letrec
	((list-make-set
	  (lambda (args-list)
	    (if (null? args-list)
		the-empty-set
		(adjoin
		 (car args-list)
		 (list-make-set (cdr args-list)))))))
      (list-make-set args))
    ))

(define none
  (lambda (pred)
    (letrec
	((test
	  (lambda (s)
	    (or (empty-set? s)
		(let ((elem (pick s)))
		  (and (not (pred elem))
		       (test ((residue elem) s))))))))
      test)))

(define there-exists
  (lambda (pred)
    (lambda (s)
      (not ((none pred) s)))
    ))

(define there-exists2
  (lambda (pred)
    (compose not (none pred))))

(define for-all
  (lambda (pred)
    (lambda (s)
      ((none (lambda (x)
	       (not (pred x)))) s))
    ))


;; subset - judge it obj1 is a subset of obj2
(define set-equal
  (lambda (obj1)
    (lambda (obj2)
      (or (and ((neither set?) obj1 obj2)
	       (equal? obj1 obj2))
	  (and ((both set?) obj1 obj2)
	       ((subset obj1) obj2)
	       ((subset obj2) obj1))))
    ))

(define set-equal?
  (lambda (obj1 obj2)
    ((set-equal obj1) obj2)))

;; test set-theoretic relation
(define element
  (lambda (obj)
    (lambda (s)
      ((there-exists (set-equal obj)) s))
    ))

(define element2
  (compose there-exists set-equal))

;; contains, set-theoretic relation
(define contains
  (lambda (set)
    (lambda (obj)
      ((elememt obj) set))))

;; if s2 is a superset of s1
(define superset
  (lambda (s1)
    (lambda (s2)
      ((for-all (contains s1)) s2))
    ))

;; test s2 is a subset of s1
(define subset
  (lambda (s1)
    (lambda (s2)
      ((superset s2) s1))))

;; number of elements in set
(define cardinal
  (lambda (s)
    (if (empty-set? s)
	0
	(let ((elem (picks)))
	  (add1 (cardinal ((residue elem) s)))))
    ))

;;
(define intersection
  (lambda (s1 s2)
    (letrec
	((helper
	  (lambda (s1)
	    (if (empty-set? s1)
		the-empty-set
		(let ((elem (pick s1)))
		  (if ((contains s2) elem)
		      (adjoin elem (helper
				    ((residue elem) s1)))
		      (helper ((residue elem) s1))))))))
      (helper s1))))

;; union
(define union
  (lambda (s1 s2)
    (letrec
	((helper
	  (lambda (s1)
	    (if (empty-set? s1)
		s2
		(let ((elem (pick s1)))
		  (if (not ((contains s2) elem))
		      (adjoin elem
			      (helper ((residue elem) s1)))
		      (helper ((residue elem) s1))))))))
      (helper s1))))

;; difference, s1\s2
(define difference
  (lambda (s1 s2)
    (letrec
	((helper
	  (lambda (s1)
	    (if (empty-set? s1)
		the-empty-set
		(let ((elem (pick s1)))
		  (if (not ((contains s2) elem))
		      (adjoin elem (helper
				    ((residue elem) s1)))
		      (helper (residue elem) s1)))))))
      (helper s1))))








;; -------------------------------------------------------
;; tests

((both (lambda (ls) (not (null? ls))))
 '(a b c)
 '(d e))

(compose not null?)
((compose not null?) '(a b c))

((at-least-one even?) 1 3)
((at-least-one even?) 1 2)

(writeln 'neither '---------------------------)
((neither zero?) 1 1)
((neither zero?) 0 0)
((both zero?) 1 1)
((both zero?) 0 0)

(writeln 'neither2)
(identity '123)
(neither2  1 1)
((neither identity) 1 1)
(neither3 1 1)

(writeln 'at-least-one)
((at-least-one one?) 1 1)
(at-least-one2 1 1)
((at-least-one one?) 0 0)
(at-least-one2 0 0)

(writeln 'both)
((both one?) 1 1)
(both2 1 1)
((both one?) 0 0)
(both2 0 0)
(make-set 1 23)

;; (let ((s (make-set 2 4 6 8 10 12)))
;;   ((none odd?) s))

(make-set 1 2 3 4 5 6 7 8 9)

(remove2 1 '(1 2 3 4)))

(let ((s (make-set 2 4 6 8 10 12)))
     ((none odd?) s))

(let ((s (make-set 1 2 3 4 5 6)))
  ((none odd?) s))

(display "Test there-exists")
((there-exists2 even?) (make-set 1 3 5 7))
((there-exists2 even?) (make-set 1 3 5 7 8))








