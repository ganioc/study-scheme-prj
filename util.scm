;; util.scm

(define sub1
  (lambda (n)
    (- n 1)))

(define add1
  (lambda (n)
    (+ n 1)))

(define writeln
  (lambda x
    (for-each display x)))

(define list-of-zeros
  (lambda (n)
    (cond
     ((zero? n) '())
     (else (cons 0 (list-of-zeros (sub1 n)))))))

;; about polynomials, in a list form
(define the-zero-poly '(0))

(define degree
  (lambda (poly)
    (sub1 (length poly))))

;; it seems zero coefficient will exist
(define leading-coef
  (lambda (poly)
    (car poly)))

(define rest-of-poly
  (lambda (poly)
    (cond
     ((zero? (degree poly)) the-zero-poly)
     ((zero? (leading-coef (cdr poly)))
      (rest-of-poly (cdr poly)))
     (else (cdr poly)))
    ))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond
       ((and (zero? deg) (equal? poly the-zero-poly))
	(list coef))
       ((>= deg-p deg)
	(error "poly-cons: Degree too high in " poly))
       ((zero? coef) poly)
       (else
	(cons coef
	      (append (list-of-zeros (sub1 (- deg deg-p)))
		      poly)))))
    ))

(define identity
  (lambda (x)
    x))
(define one?
  (lambda (x)
    (not (zero? x))))

(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     (else (or (equal? (car ls) item)
	       (member? item (cdr ls)))))
    ))

;; (define compose
;;   (lambda (f g)
;;     (lambda (x)
;;       (f (g x)))
;;     ))
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

(define remove-1st
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     (else (cons (car ls) (remove-1st item (cdr ls)))))
    ))

(define singleton-list?
  (lambda (ls)
    (and (pair? ls) (null? (cdr ls)))
    ))
		     
(define merge
  (lambda (sorted-ntpl1 sorted-ntpl2)
    (cond
     ((null? sorted-ntpl1) sorted-ntpl2)
     ((null? sorted-ntpl2) sorted-ntpl1)
     ((< (car sorted-ntpl1) (car sorted-ntpl2))
      (cons (car sorted-ntpl1)
	    (merge (cdr sorted-ntpl1) sorted-ntpl2)))
     (else
      (cons (car sorted-ntpl2)
	    (merge sorted-ntpl1 (cdr sorted-ntpl2)))))
    ))

(define set-tag "set")
(define the-empty-set (cons set-tag '()))
(define empty-set?
  (lambda (s)
    (eq? s the-empty-set)))
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
  

(define for-all
  (lambda (pred)
    (none (compose not pred))
    ))

(define 1st
  (lambda (ls)
    (car ls)))

(define 2nd
  (lambda (ls)
    (cadr ls)))
(define 3rd
  (lambda (ls)
    (caddr ls)))
(define 4th
  (lambda (ls)
    (cadddr ls)))
