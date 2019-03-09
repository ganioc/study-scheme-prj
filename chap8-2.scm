;; chap8-2

(load "util.scm")

(define both
  (lambda (pred)
    (lambda (arg1 arg2)
      (and (pred arg1) (pred arg2)))
    ))

(define neither
  (lambda (pred)
    (lambda (arg1 arg2)
      (not (or (pred arg1) (pred arg2))))
    ))

(define at-least-one
  (lambda (pred)
    (lambda (arg1 arg2)
      (or (pred arg1) (pred arg2)))
    ))

(define set-tag "set")
(define the-empty-set (cons set-tag '()))

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
      (list-make-set args))))

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
    (compose not (none pred))))

(define for-all
  (lambda (pred)
    (none (compose not pred))))

(define set-equal
  (lambda (obj1)
    (lambda (obj2)
      (or (and ((neither set?) obj1 obj2)
	       (equal? obj1 obj2))
	  (and ((both set?) obj1 obj2)
	       ((subset obj1) obj2)
	       ((subset obj2) obj1))))
    ))

(define element
  (compose there-exists set-equal))

(define contains
  (lambda (set)
    (lambda (obj)
      ((element obj) set))
    ))

(define superset
  (lambda (s1)
    (lambda (s2)
      ((for-all (contains s1)) s2))
    ))

(define subset
  (lambda (s1)
    (lambda (s2)
      ((superset s2) s1))
    ))

(define cardinal
  (lambda (s)
    (if (empty-set? s)
	0
	(let ((elem (pick s)))
	  (add1 (cardinal ((residue elem) s)))))
    ))

(define intersection
  (lambda (s1 s2)
    (letrec
	((helper
	  (lambda (s1)
	    (if (empty-set? s1)
		the-empty-set
		(let ((elem (pick s1)))
		  (if ((contains s2) elem)
		      (adjoin elem (helper ((residue elem) s1)))
		      (helper ((residue elem) s1))))))))
      (helper s1))))

(define union
  (lambda (s1 s2)
    (letrec
	((helper
	  (lambda (s1)
	    (if (empty-set? s1)
		s2
		(let ((elem (pick s1)))
		  (if (not ((contains s2) elem))
		      (adjoin elem (helper ((residue elem) s1)))
		      (helper ((residue elem) s1))))))))
      (helper s1))))

(define difference
  (lambda (s1 s2)
    (letrec
	((helper
	  (lambda (s1)
	    (if (empty-set? s1)
		the-empty-set
		(let ((elem (pick s1)))
		  (if (not ((contains s2) elem))
		      (adjoin elem (helper ((residue elem) s1)))
		      (helper ((residue elem) s1))))))))
      (helper s1))
    ))

(define symmetric-difference
  (lambda (s1 s2)
    (union (difference s1 s2)
	   (difference s2 s1))
    ))

(define set-builder
  (lambda (pred base-set)
    (letrec
	((helper
	  (lambda (s)
	    (if (empty-set? s)
		base-set
		(let ((elem (pick s)))
		  (if (pred elem)
		      (adjoin elem (helper ((residue elem) s)))
		      (helper ((residue elem) s))))))))
      helper)))

(define family-union
  (lambda (s)
    (if (empty-set? s)
	the-empty-set
	(let ((elem (pick s)))
	  (union elem (family-union ((residue elem) s)))))
    ))

(define family-intersection
  (lambda (s)
    (if (empty-set? s)
	the-empty-set
	(letrec
	    ((fam-int
	      (lambda (s)
		(let ((elem (pick s)))
		  (let ((rest ((residue elem) s)))
		    (if (empty-set? rest)
			elem
			(intersection elem (fam-int rest))))))))
	  (fam-int s)))
    ))

(define set-map
  (lambda (proc s)
    (if (empty-set? s)
	the-empty-set
	(let ((elem (pick s)))
	  (adjoin (proc elem)
		  (set-map proc ((residue elem) s)))))
    ))

(define list->set
  (lambda (ls)
    (apply make-set ls)))

(define set->list
  (lambda (s)
    (if (empty-set? s)
	'()
	(let ((elem (pick s)))
	  (cons elem (set->list ((residue elem) s)))))
    ))

(define empty-set?
  (lambda (s)
    (eq? s the-empty-set)))

(define set?
  (lambda (arg)
    (and (pair? arg) (eq? (car arg) set-tag))))

(define pick
  (lambda (s)
    (let ((ls (cdr s)))
      (if (null? ls)
	  (error "pick: The set is empty.")
	  (list-ref ls (random (length ls)))))
    ))

(define residue
  (lambda (elem)
    (lambda (s)
      (let ((ls (remove-1st elem (cdr s))))
	(cond
	 ((null? ls) the-empty-set)
	 (else (cons set-tag ls)))))
    ))

;; P277, adjoin set
(define adjoin
  (lambda (elem s)
    (cond
     ((member? elem (cdr s)) s)
     (else (cons set-tag (cons elem (cdr s)))))
    ))
(define map-set
  (lambda (pred s)
    (if (empty-set? s)
	(make-set the-empty-set)
	(letrec ((item (pick s)))
	  (adjoin (pred item) (map-set pred ((residue item) s)))))
    ))

;; power-set
(define power-set
  (lambda (s)
    (if (empty-set? s)
	(make-set the-empty-set)
	(let* ((a (pick s))
	       (rest ((residue a) s)))
 	  ;; (writeln "a:" a " rest:" rest)
	  ;; (newline)
	  (union
	   (power-set rest)
	   (map-set (lambda (x)
		  (adjoin a x))
		(power-set rest)))))
    ))

(define power-ls2
  (lambda (ls)
    (if (null? ls)
	'(())
	(append
	 (map (lambda (x)
		(cons (car ls) x))
	      (power-ls2 (cdr ls)))
	 (power-ls (cdr ls))))
    ))

;; Is this the correct way of doing this?
(define power-ls
  (lambda (ls)
    (if (null? ls)
	'(())
	(let ((rest (power-ls (cdr ls))))
	  (append
	   rest
	   (map (lambda (x)
		  (cons (car ls) x))
		rest))))
    ))
	  
	  
(define select-by-cardinal
  (lambda (num)
    (letrec ((helper
	      (lambda (s)
		(writeln "helper")
		(newline)
		(if (empty-set? s)
		    the-empty-set
		    (let* ((a (pick s))
			   (rest ((residue a) s)))
		      (if (= num (cardinal a))
			  (union (make-set a) (helper rest))
			  (helper rest)))
		))))
      
      helper)))




;; ordered pair

;; defined by set
(define make-op
  (lambda (x y)
    (make-set (make-set x) (make-set x y))
    ))

(define op?
  (lambda (set)
    (and (set? set)
	 ((for-all set?) set)
	 (= (cardinal (family-intersection set)) 1)
	 (or (= (cardinal set) 1)
	     ((both (lambda (x) (= (cardinal x) 2)))
	      set
	      (family-union set))))))

(define op-1st
  (lambda (op)
    (pick (family-intersection op))
    ))

(define op-2nd
  (lambda (op)
    (let ((fam-int (family-intersection op)))
      (let ((diff (difference (family-union op) fam-int)))
	(pick (if (empty-set? diff) fam-int diff))))
    ))

(define make-op
  (lambda (x y)
    (list x y)))

(define op?
  (lambda (ls)
    (and (pair? ls) (pair? (cdr ls)) (null? (cdr ls)))
    ))

(define op-1st
  (lambda (op)
    (car op)))

(define op-2nd
  (lambda (op)
    (cadr op)))

(define cartesian-product
  (lambda (s1 s2)
    (if (empty-set? s1)
	the-empty-set
	(let ((elem (pick s1)))
	  (union
	   (set-map (lambda (x)
		      (make-op elem x)) s2)
	   (cartesian-product
	    ((residue elem) s1) s2))))
    ))




;; tests
(writeln "-----------------------------------")
((neither null?) '(a b c) '(d e))
(list->set '(a b a b))

((set-equal (list->set '(a b a b)))
 (list->set '(b a a)))
 
((element 'a) (make-set (list->set '(a))
			(list->set '(a a))
			(list->set '(a a a))))

((element 'a) (make-set (list->set '(a))
			'a
			(list->set '(a a))))

(union (make-set 1 1 2 3 4)
       (make-set 3 4 4 5 6 6))

(intersection (make-set 1 2 3 3 4 5)
	      (make-set 3 4 4 5 6 7))

(writeln "difference")
(difference (make-set 1 1 2 3 3 4 5)
	    (make-set 3 4 4 5 6 7))

(symmetric-difference (make-set 1 1 2 3 3 4 5)
	    (make-set 3 4 4 5 6 7))

(set-map cardinal (make-set (list->set '(a b c))
			    (list->set '(a b a))
			    (list->set '(a a a))
			    (list->set '())))

(family-intersection (make-set (list->set '(a b c d d))
			       (list->set '(a c d e))
			       (list->set '(c d e f))))


(make-set (list->set '(a b c))
			    (list->set '(a b a))
			    (list->set '(a a a))
			    (list->set '()))

(writeln "power-set")

;;(power-set (make-set '()))
(make-set '())
the-empty-set
(make-set 'a 'b 'c)
(make-set (make-set 'a 'b 'c)
	  the-empty-set
	  (make-set 'a)
	  (make-set 'b)
	  (make-set 'a 'b))

(list-ref '(a b c) 0)
(length '(a b c))

(power-set (make-set 'a 'b 'c))


(map (lambda (x)
       (cons 'x x))
     '(a b c))

(append '( 1 2 3) '(3 4))

(null? '(a))
(null? (cdr '(a)))
(null? (car '(a)))
(car '(a))


(pair? (list 'a))

(append (map (lambda (x)
	       (cons 'x x)) '(a b c))
	'((1 2 3 4) (8 9)))

;;(power-ls '(a b c))
(power-ls '(a))
(power-ls '(a b))
(power-ls '(a b c))
(power-ls2 '(a b c))
;; (power-set '(a b c))

(make-set the-empty-set)
(map-set (lambda (x)
	   (make-set 'x x))
	 (make-set 'a 'b 'c))
;;(power-set (make-set 'a 'b 'c))
(make-set 'a)
(adjoin  'a  (make-set 'c))

(union (make-set (make-set 'a) (make-set 'b)) (make-set 'c 'd))

(power-set (make-set 'a 'b))		     
(power-set (make-set 'a 'b 'c))

((select-by-cardinal 2)
 (make-set (make-set 'a)
	   (make-set 'a 'b)
	   (make-set 'a 'b 'c)
	   (make-set 'b 'c)
	   (make-set 'b)))

(cartesian-product (make-set 'a 'b 'c)
		   (make-set 'd 'e))
