;; arithmetic, data abatraction

(define numr
  (lambda (rtl)
    (car rtl)
    ))

(define denr
  (lambda (rtl)
    (cadr rtl)
    ))

(define make-ratl
  (lambda (in1 in2)
    (if (= 0 in2)
	(error "Error make-ratl: The denr cannot be zero.")
	(list (/ in1 (gcd in1 in2)) (/ in2 (gcd in1 in2))))
    ))

(make-ratl 2 4)

(define rzero?
  (lambda (rtl)
    (= 0 (numr rtl)))
  )

(define r+
  (lambda ( x y)
    (make-ratl
     (+ (* (numr x) (denr y)) (* (numr y) (denr x)))
     (* (denr x) (denr y)))
    )
  )

(define r*
  (lambda (x y)
    (make-ratl
     (* (numr x) (numr y))
     (* (denr x) (denr y)))
    )
  )

(define r-
  (lambda (x y)
    (make-ratl
     (- (* (numr x) (denr y)) (* (numr y) (denr x)))
     (* (denr x) (denr y)))
    )
  )

(define rinvert
  (lambda (rtl)
    (if (rzero? rtl)
	(error "Error: rinvert cannot convert " rtl)
	(make-ratl (denr rtl) (numr rtl)))
    )
  )

(define r/
  (lambda (x y)
    (r* x (rinvert y))
    )
  )

(define r=
  (lambda (x y)
    (= (* (numr x) (denr y)) (* (numr y) (denr x)))
    )
  )

(define rpositive?
  (lambda (rtl)
    (or (and (positive? (numr rtl)) (positive? (denr rtl)))
	(and (negative? (numr rtl)) (negative? (denr rtl))))
    )
  )

(define rnegative?
  (lambda (rtl)
    (or (and (positive? (numr rtl)) (negative? (denr rtl)))
	(and (negative? (numr rtl)) (positive? (denr rtl))))
    ))
	     


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
    (writeln (numr rtl) "/" (denr rtl))
    )
  )

(define rminus
  (lambda (rtl)
    (make-ratl (* -1 (numr rtl)) (denr rtl))
    ))

(define rsame-sign?
  (lambda (rtl1 rtl2)
    (or (and (rpositive? rtl1) (rpositive? rtl2))
	(and (rnegative? rtl1) (rnegative? rtl2)))
    ;; (or (and (rpositive? rtl1) (rpositive? rtl2))
    ;; 	(and (rnegative? rtl1) (rnegative? rtl2)))
    ))

(define rabs
  (lambda (rtl)
    (if (rnegative? rtl)
	(rminus rtl)
	rtl)
    ))


;; test goes here -------------------
(define temp
  (lambda ()
    (make-ratl 1 3)
    ))

(rprint temp)
	
(rprint (r+ (make-ratl 1 4) (make-ratl 2 3)))

;; (rprint (rminus temp))
(= 0 1)
(= 0 -1)
(= 0 0)
;; (rminus temp)
(make-ratl 1 2)
(make-ratl -1 1)
(rminus (temp))
(rsame-sign? (temp) (temp))
;; (same-sign? (temp) (rminus (temp)))
(rminus (make-ratl 1 2))
(rsame-sign? (make-ratl 1 2) (make-ratl 2 3))

(rminus (make-ratl -2 2))

(define n1
  (make-ratl 1 2))
(define n2
  (make-ratl 2 2))

(rminus n1)
(rminus n2)
(rpositive? (rminus n1))
(rpositive? n1)

(rsame-sign? (rminus (make-ratl 2 2)) (make-ratl 2 3))
(rabs n1)
(rabs (rminus n1))

(make-ratl 12 20)
(make-ratl -10 15)

						      
			 




