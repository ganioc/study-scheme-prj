;; vecotr, customized vector definition, home-made
(load "util.scm")
;; vecotr definition, car is the tag, cdr is a pair (length, procedure)
;; I will
;; 实现一个生成函数
;; 生成一个索引函数
;; 
;; Use 2nd version ,use list to contain elements
;; (tag (size, list))

(define vector-tag "vector")

(define vector2-generator
  (lambda (gen-proc)
    (lambda (size)
      (cons vector-tag
	    (cons size
		  (letrec
		      ((loop (lambda (i)
			       (cond
				((= i size) '())
				(else (cons (gen-proc i)
					    (loop (add1 i))))))))
		    (loop 0)))))
    ))

(define vector2-ref
  (lambda (vec i)
    (list-ref (cddr vec) i)
    ))

;; (define vector2
;;   (lambda args
;;     (cond
;;      ((number? (car args))
;;       ((vector2-generator
;; 	(lambda (i) 0)) (car args)))
;;      ((number? (cadr args))
;;       ((vector2-generator
;; 	(car args)) (cadr args)))
;;      (else
;;       (writeln (car args))
;;       (writeln (cadr args))
;;       (error "Error: It is unknown")))
;;     ))

(define vector2-length
  (lambda (vec)
    (car (cdr vec))
    ))
(define view
  (lambda (vec)
    (let ((highest-index (sub1 (vector2-length vec))))
      (letrec ((loop (lambda (i)
		       (display (vector2-ref vec i))
		       (if (< i highest-index)
			   (begin
			     (display " ")
			     (loop (add1 i)))))))
	(display "#(")
	(loop 0)
	(display ")")))
    ))

(define dot-product
  (lambda (vec1 vec2)
    (let ((size (vector2-length vec1)))
      (letrec
	  ((loop
	    (lambda (i acc)
	      (if (= i size)
		  acc
		  (loop (add1 i)
			(+ acc (* (vector2-ref vec1 i)
				  (vector2-ref vec2 i))))))))
	(loop 0 0)))
    ))
(define make-vector2
  (lambda args
    (let ((fill-value
	   (if (singleton-list? args)
	       '()
	       (cadr args))))
      ((vector2-generator (lambda (i) fill-value)) (car args)))))

(define list->vector2
  (lambda (ls)
    ((vector2-generator (lambda (i) (list-ref ls i)))
     (length ls))
    ))
(define vector2
  (lambda args
    (list->vector2 args)))
(define vector2-stretch
  (lambda (vec new-size)
    (let ((size (vector2-length vec)))
      (let ((gen-proc (lambda (i)
			(if (< i size)
			    (vector2-ref vec i)
			    '()))))
	((vector2-generator gen-proc) new-size)))
    ))
(define vector2-copy
  (lambda (vec)
    (vector2-stretch vec (vector2-length vec))))
(define vector2-update
  (lambda (vec k val)
    (let ((gen-proc (lambda (i)
		      (if (= i k)
			  val
			  (vector2-ref vec i)))))
      ((vector2-generator gen-proc) (vector2-length vec)))))
(define vector2-map
  (lambda (proc vec)
    ((vector2-generator (lambda (i)
			  (proc (vector2-ref vec i))))
     (vector2-length vec))))
(define multiply-by-scalar
  (lambda (c vec)
    (vector2-map (lambda (elem)
		   (* c elem)) vec)))

(define vector2-apply-elementwise-to-both
  (lambda (proc)
    (lambda (vec1 vec2)
      (let ((gen-proc
	     (lambda (i)
	       (proc (vector2-ref vec1 i) 
		     (vector2-ref vec2 i)))))
	((vector2-generator gen-proc) (vector2-length vec1))))
    ))
(define vec2+
  (vector2-apply-elementwise-to-both +))
(define vec2-
  (vector2-apply-elementwise-to-both -))
(define vec2*
  (vector2-apply-elementwise-to-both *))
(define vector2-sum
  (lambda (vec)
    (let ((size (vector2-length vec)))
      (letrec
	  ((helper
	    (lambda (i)
	      (if (= i size)
		  0
		  (+ (vector2-ref vec i)
		     (helper (add1 i)))))))
	(helper 0)))
    ))
(define vector2-product
  (lambda (vec)
    (let ((size (vector2-length vec)))
      (letrec
	  ((helper
	    (lambda (i)
	      (if (= i size)
		  1
		  (* (vector2-ref vec i)
		     (helper (add1 i)))))))
	(helper 0)))
    ))
(define vector2-accumulate
  (lambda (proc seed)
    (lambda (vec)
      (let ((size (vector2-length vec)))
	(letrec
	    ((helper
	      (lambda (i)
		(if (= i size)
		    seed
		    (proc (vector2-ref vec i)
			  (helper (add1 i)))))))
	  (helper 0))))
    ))
(define vector2->list
  (vector2-accumulate cons '()))

(define dot-product2 ;; (compose vector2-sum vec2*))
  (lambda (vec1 vec2)
    (vector2-sum (vec2* vec1 vec2))))

  




;; test
(writeln "--------------------Start-------------------")
;;(vector2 (lambda (i) (* i i)) 6))
((vector2-generator (lambda (i) i)) 6)
(vector2 6)
(define vec6
  (vector2 6))
(vector2-length vec6)
(vector2-ref vec6 0)
(view vec6)
(list->vector2 '(10 11 12 13 14 15))
(make-vector2 5)
(define vec7
  (vector2 6 'symbol 5))
(view (vector2 6 'symbol 5))
(vector2-stretch vec6 3)
(vector2-copy vec6)
(vector2-update vec6 0 100)
(vector2-update vec7 1 33)
(view (vector2-map add1 (vector2 10 11 12 13)))
(view (vector2-map even? (vector2 10 11 12)))
(view (vector2-map
       (lambda (elem) (list 'a elem))
       (vector2 10 11 12 13 14)))
(multiply-by-scalar 4 (vector2 10 11 12))
((vector2-apply-elementwise-to-both
  (lambda (item1 item2) (+ item1 item2)))
 (vector2 1 2 3 4)
 (vector2 4 4 4 4))
(view (vec2+ (vector2 1 3 5 7 9) (vector2 9 7 5 3 1)))
(view (vec2- (vector2 1 3 5 7 9) (vector2 9 7 5 3 1)))
(view (vec2* (vector2 1 3 5 7 9) (vector2 9 7 5 3 1)))
(vector2-sum (vector2 1 3 5 7 9))
(vector2-product (vector2 1 3 5 7 9))
((vector2-accumulate + 0) (vector2 1 3 5 7 9))
((vector2-accumulate * 1) (vector2 1 3 5 7 9))
(vector2->list (vector2 2 3 4))
(dot-product2 (vector2 2 2) (vector2 1.1 2.2))
((compose vector2-sum vec2*) (vector2 2 2) (vector2 1.1 2.2))

(Writeln "--------------------End-------------------")
