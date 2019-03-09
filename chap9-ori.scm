;; chap9 original vector
(load "util.scm")

;; test
(define vec1
  #(a b c))

;; available
(vector-length vec1)
(vector-ref vec1 0)
(vector-ref vec1 1)
(vector-ref vec1 2)
;; (vector-ref vec1 3)

;; Not available
;; (view vec1)

;; not available
;;((vector-generator (lambda (i) 0)) 6)

;; available
(list->vector '(1 2 3 4))

;; available
(vector 1 2 3)
(vector 6 'symbol 5)

(define vecnum #(5 6 7 8))
;; available
(make-vector 5 2 )
(make-initialized-vector 5 (lambda (x) (* x x)))
(vector-grow vec1 5)
(vector-map (lambda (x) (+ x 10)) vecnum)

;; not available
;; (string->vector "google" 1 3)
;; (vector->string #(4 5 6))

(define vector-generator
  (lambda (gen-proc)
    (lambda (size)
      (let ((vec (make-vector size)))
	(letrec
	    ((loop (lambda (i)
		     (if (< i size)
			 (begin
			   (vector-set! vec i (gen-proc i))
			   (loop (add1 i)))))))
	  (loop 0))
	vec))
    ))

(define vector-update!
  (lambda (vec i c)
    (vector-set! vec i c)
    vec))

(define vector-update
  (lambda (vec i1 c)
    (let ((size (vector-length vec)))
      (letrec
	  ((loop (lambda (i)
		   (cond
		    ((= i 0)
		     (list->vector
		      (append (if (= i i1)
				  (list c)
				  (list (vector-ref vec i)))
			      (loop (add1 i)))))
		    ((= i1 i)
		     (append (list c)
			     (loop (add1 i))))
		    ((< i size)
		     (append (list (vector-ref vec i))
			     (loop (add1 i))))
		    (else '())))))
	(loop 0)))
    ))
(define swap-maker
  (lambda (vec)
    (lambda (index1 index2)
      (let ((temp (vector-ref vec index1)))
	(vector-update
	 (vector-update vec index1 (vector-ref vec index2))
	 index2
	 temp)))
    ))
		   
(define vector-reverse
  (lambda (vec)
    (letrec
	((switch
	  (lambda (v i j)
	    (if (>= i j)
		v
		(let ((swapv (swap-maker v)))
		  (switch (swapv i j) (add1 i) (sub1 j)))))))
      (switch vec 0 (sub1 (vector-length vec))))
    ))
(define swap-maker!
  (lambda (vec)
    (lambda (index1 index2)
      (let ((temp (vector-ref vec index1)))
	(vector-update!
	 (vector-update! vec index1 (vector-ref vec index2))
	 index2
	 temp)))
    ))
(define vector-reverse!
  (lambda (vec)
    (let ((swapv! (swap-maker! vec)))
      (letrec
	  ((switch (lambda (i j)
		     (if (< i j)
			 (begin
			   (swapv! i j)
			   (switch (add1 i) (sub1 j)))))))
	(switch 0 (sub1 (vector-length vec))))
      vec)))

(define successive-powers
  (lambda (base)
    (lambda (times)
      (letrec
	  ((loop
	    (lambda (i)
	      (cond
	       ((= i 0)
		(list->vector
		 (append
		  (list (expt base i))
		  (loop (add1 i)))))
	       ((= i (- times 1))
		(list (expt base i)))
	       (else
		(append
		 (list (expt base i))
		 (loop (add1 i))))))))
	(loop 0)))
    ))
 
;; test -----------------------------------
(writeln "-----------------------------------")
'#(10 u (+ 2 3) "Mary")
(writeln '#(10 20 (+ 10 20)))
(vector 10 20 (+ 10 20) 40 50)

(define v1 (vector 0 2 4 6 8))

v1

(vector-set! v1 2 5)

v1 
(let ((a (vector 2 4 6)))
  (let ((b (vector-update! a 1 5)))
    (writeln a)
    (newline)
    (writeln b)
    (newline)
    ))

(let ((a (vector 2 4 6)))
  (let ((b (vector-update a 1 5)))
    (writeln a)
    (newline)
    (writeln b)
    (newline)
    ))

(list->vector '(1 2 3 4))
(list->vector (list 1 2))

(vector-update v1 0 9)
(let ((a (vector 1 2 3 4 5)))
  (let ((b (vector-reverse a)))
    (writeln a) (newline)
    (writeln b) (newline)
    ))

(vector-update v1 0 9)
(let ((a (vector 1 2 3 4 5)))
  (let ((b (vector-reverse! a)))
    (writeln a) (newline)
    (writeln b) (newline)
    ))

((successive-powers 2) 8)
((successive-powers 3) 5)
