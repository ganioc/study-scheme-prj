;; chapter 5
(load "util.scm")

(define a 10)

(define b 2)

(let ((a (+ a 5)))
  (* a b))

(let ((a 10)
      (b 2))
  (let ((a (+ a 5)))
    (* a b)))

(define addb
  (let ((b 100))
    (lambda (x)
      (+ x b))))

(let ((b 10))
  (addb 25))

(let ((b 2)) ;; environment 1
  (let ((add2 (lambda (x)  ;; environment 2
		(+ x b)))
	(b 0.5))
    (/ b (add2 b))))

(define remove-leftmost
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     ((pair? (car ls))
      (let ((rem-list (remove-leftmost item (car ls))))
	(cons rem-list (cond
			((equal? (car lst) rem-list)
			 (remove-leftmost item (cdr ls)))
			(else (cdr ls))))))
     (else (cons (car ls) (remove-leftmost item (cdr ls)))))
    ))


(letrec ((fact (lambda (n)
	      (if (zero? n)
		  1
		  (* n (fact (sub1 n)))))))
  (fact 4))

(letrec ((even? (lambda (x)
		  (or (zero? x) (odd? (sub1 x)))))
	 (odd? (lambda (x)
		 (and (not (zero? x)) (even? (sub1 x))))))
  (odd? 17))

(define fact3
  (lambda (n)
    (letrec ((fact-it
	      (lambda (k acc)
		(if (zero? k)
		    acc
		    (fact-it (sub1 k) (* k acc))))))
    (fact-it n 1))
    ))
      
(define swapper
  (lambda (x y ls)
    (letrec ((swap (lambda (ls*)
		     (cond
		      ((null? ls*) '())
		      ((equal? (car ls*) x) (cons y (swap (cdr ls*))))
		      ((equal? (car ls*) y) (cons x (swap (cdr ls*))))
		      (else (cons (car ls*) (swap (cdr ls*))))))))
      (swap ls))
    ))
	
(let ((a 5))
  (let ((fun (lambda (x) (max x a))))
    (let ((a 10)
	  (x 20))
      (fun 1))))

(let ((a 1)
      (b 2))
  (let ((b 3)
	(c (+ a b)))
    (let ((b 5))
      (cons a (cons b (cons c '()))))))

(letrec
    ((loop (lambda (n k)
	     (cond
	      ((zero? k) n)
	      ((< n k) (loop k n))
	      (else (loop k (remainder n k)))))))
  (loop 9 12))

(letrec
    ((loop (lambda (n)
	     (if (zero? n)
		 0
		 (+ (remainder n 10) (loop (quotient n 10)))))))
  (loop 1234))

(letrec ((mystery
	  (lambda (tuple odds evens)
	    (if (null? tuple)
		(append odds evens)
		(let ((next-int (car tuple)))
		  (if (odd? next-int)
		      (mystery (cdr tuple)
			       (cons next-int odds) evens)
		      (mystery (cdr tuple)
			       odds (cons next-int evens))))))))
  (mystery '(3 16 4 7 9 12 24) '() '()))

(define mystery2
  (lambda (n)
    (letrec ((mystery-helper
	      (lambda (n s)
		(writeln "n:" n "  s:" s)
		(newline)
		(cond
		 ((zero? n) (list s))
		 (else
		  (begin
		    (newline)
		    (writeln "To append -->")
		    (newline)
		    (append
		     (begin
		       (writeln "up")
		       (newline)
		       (mystery-helper (sub1 n) (cons 0 s)))
		     ;;'(*)
		     (begin
		       (writeln "down")
		       (newline)
		       (mystery-helper (sub1 n) (cons 1 s))))))))))
      (mystery-helper n '()))))

;; plynomials
(define zero-poly?
  (lambda (poly)
    (and (zero? (degree poly)) (zero? (leading-coef poly)))
    ))

(define make-term
  (lambda (deg coef)
    (poly-cons deg coef the-zero-poly)))

(define leading-term
  (lambda (poly)
    (make-term (degree poly) (leading-coef poly))))

(define p+
  (lambda (poly1 poly2)
    (cond
     ((zero-poly? poly1) poly2)
     ((zero-poly? poly2) poly1)
     (else (let ((n1 (degree poly1))
		 (n2 (degree poly2))
		 (a1 (leading-coef poly1))
		 (a2 (leading-coef poly2))
		 (rest1 (rest-of-poly poly1))
		 (rest2 (rest-of-poly poly2)))
	     (cond
	      ((> n1 n2) (poly-cons n1 a1 (p+ rest1 poly2)))
	      ((< n1 n2) (poly-cons n2 a2 (p+ poly1 rest2)))
	      (else
	       (poly-cons n1 (+ a1 a2) (p+ rest1 rest2)))))))
    ))

(define p*
  (letrec
      ((t* (lambda (trm poly)
	     (if (zero-poly? poly)
		 the-zero-poly
		 (poly-cons
		  (+ (degree trm) (degree poly))
		  (* (leading-coef trm) (leading-coef poly))
		  (t* trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
	  ((p*-helper (lambda (p1)
			(if (zero-poly? p1)
			    the-zero-poly
			    (p+ (t* (leading-term p1) poly2)
				(p*-helper (rest-of-poly p1)))))))
	(p*-helper poly1)))))

(define negative-poly
  (lambda (poly)
    (let ((poly-negative-one (make-term 0 -1)))
      (p* poly-negative-one poly))))

(define p-
  (lambda (poly1 poly2)
    (p+ poly1 (negative-poly poly2))))

(define poly-value
  (lambda (poly num)
    (letrec
	((pvalue (lambda (p)
		   (let ((n (degree p)))
		     (if (zero? n)
			 (leading-coef p)
			 (let ((rest (rest-of-poly p)))
			   (if (< (degree rest) (sub1 n))
			       (pvalue (poly-cons
					(sub1 n)
					(* num (leading-coef p))
					rest))
			       (pvalue (poly-cons
					(sub1 n)
					(+ (* num (leading-coef p))
					   (leading-coef rest))
					(rest-of-poly rest))))))))))
      (pvalue poly))
    ))
			   
(define poly-quotient
  (lambda (poly1 poly2)
    (letrec
	((deg1 (degree poly1))
	 (deg2 (degree poly2))
	 (coef1 (leading-coef poly1))
	 (coef2 (leading-coef poly2))
	 (quoti (quotient (leading-coef poly1)
			  (leading-coef poly2)))
	 (increase-degree (lambda (poly deg)
			    (if (< deg 1)
				(error "Error minus deg"))
			    (append poly (list-of-zeros deg))))
	 )

      (cond
       ((< deg1 deg2) poly1)
       ((= deg1 deg2) (list quoti))
       (else
	(let* ((degdiff (- deg1 deg2))
	       (temp1 (increase-degree (p* (list quoti) poly2)
				       degdiff))
	       (temp2 (p- poly1 temp1)))

	  (p+ (increase-degree (list quoti) degdiff)
	      (poly-quotient temp2 poly2))
	  )
	
	)))
    ))
		 
(define poly-remainder
  (lambda (poly1 poly2)
    (p- poly1 (p* poly2 (poly-quotient poly1 poly2)))
    ))

  

;; Here goes the test
(writeln "Here goes the test -->")
(fact3 5)
(remainder 5 10)
(quotient 5 10)
(remainder 2 7)
(remainder 3 6)
(remainder 20 5)
(quotient 20 5)
(mystery2 1)
(mystery2 2)

the-zero-poly

(define p1 (poly-cons 3 5 the-zero-poly))

p1

(poly-value p1 2)
(p* '(2) '(1 1))
(poly-quotient '(5 2 1 1) '(1 1))
(poly-remainder '(5 2 1 1) '(1 1))
