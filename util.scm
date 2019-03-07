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

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))
    ))

(define remove-1st
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) item) (cdr ls))
     (else (cons (car ls) (remove-1st item (cdr ls)))))
    ))

