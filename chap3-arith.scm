;; arithmetic, data abatraction


(exact->inexact (/ 3 7))

(exact? (/ 1 3))
(define n1 (exact->inexact (/ 1 3)))

(+ 1 n1)

(define n2 (/ 1 3))

(+ 1 n2)

(numerator n2)
(denominator n2)
(/ (numerator n2) (denominator n2))

(define make-ratl
  (lambda (numr denr)
    (/ numr denr)))

(make-ratl 2 4)

(define rzero?
  (lambda (rtl)
    (zero? (numerator rtl)))
  )

(define r+
  (lambda ( x y)
    (make-ratl
     (+ (* (numerator x) (denominator y)) (* (numerator y) (denominator x)))
     (* (denominator x) (denominator y)))
    )
  )

(define r*
  (lambda (x y)
    (make-ratl
     (* (numerator x) (numerator y))
     (* (denominator x) (denominator y)))
    )
  )

(define r-
  (lambda (x y)
    (make-ratl
     (- (* (numerator x) (denominator y)) (* (numerator y) (denominator x)))
     (* (denominator x) (denominator y)))
    )
  )

(define rinvert
  (lambda (rtl)
    (if (rzero? rtl)
	(error "Error: rinvert cannot convert " rtl)
	(make-ratl (denominator rtl) (numerator rtl)))
    )
  )

(define r/
  (lambda (x y)
    (r* x (rinvert y))
    )
  )

(define r=
  (lambda (x y)
    (= (* (numerator x) (denominator y)) (* (numerator y) (denominator x)))
    )
  )

(define rpositive?
  (lambda (rtl)
    (or (and (positive? (numerator rtl)) (positive? (denominator rtl)))
	(and (negative? (numerator rtl)) (negative? (denominator rtl))))
    )
  )

(define r>
  (lambda (x y)
    (rpositive? (r- x y))
    )
  )

(define rmax
  (lambda (x y)
    (if (r> x y)
	x
	y)
    )
  )

;; (define rmin
;;   (lambda (x y)
;;     (if (equal? x (rmax x y))
;; 	y
;; 	x)
;;     )
;;   )

(define extreme-value
  (lambda (pred x y)
    (if (pred x y)
	x
	y)
    )
  )

(define rmin
  (lambda (x y)
    (extreme-value r< x y)
    )
  )

(min 100 123)

(define writeln
  (lambda x
    (for-each display x)
    )
  )

(define rprint
  (lambda (rtl)
    (writeln (numerator rtl) "/" (denominator rtl))
    )
  )


	

						      
			 




