;; chapter 3 numbers

(/ 2 3)

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define harmonic-sum
  (lambda (n)
    (cond
     ((= 0 n) 0)
     (else (+ (/ 1 n) (harmonic-sum (sub1 n))))
     )
    )
  )

(define list-of-zeros
  (lambda (n)
    (cond
     ((zero? n) '())
     (else (cons 0 (list-of-zeros (sub1 n)))))))

(harmonic-sum 5)
(zero? 1)

(list-of-zeros 10)
(length '(1 4 5 7))
(list-ref '(1 4 5 7) 2)
;; (error "Some error")

(define pairwise-sum
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) '())
     (else
      (cons (+ (car ls1) (car ls2)) (pairwise-sum (cdr ls1) (cdr ls2)))))))

(pairwise-sum '(1 3 2) '(4 -1 2))


(define dot-product
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) 0)
     (else
      (+ (* (car ls1) (car ls2)) (dot-product (cdr ls1) (cdr ls2)))))))

(dot-product '(3 4 -1) '(1 -2 -3))

(define mult-by-n
  (lambda (n ls)
    (cond
     ((null? ls) '())
     (else
      (cons (* n (car ls)) (mult-by-n n (cdr ls)))))))

(mult-by-n 3 '(1 2 3 4 5 6))

(define index-handle
  (lambda (i n lst)
    (cond
     ((null? lst) -1)
     ((equal? (car lst) n) i)
     (else (index-handle (add1 i) n (cdr lst))))
    )
  )

(define index
  (lambda (n lst)
    (cond
     ((null? lst) -1)
     (else (index-handle 0 n lst))
     )))

(index 3 '(1 2 3 4 5 6))
(index 'so '(do re me fa so la ti do))
(index 'a '(b c d e))

(define make-list
  (lambda (n item)
    (cond
     ((equal? 0 n) '())
     (else (cons item (make-list (sub1 n) item))))
    )
  )

(make-list 5 'no)
(make-list 1 'maybe)
(make-list 0 'yes)
(length (make-list 7 'any))

(define count-background
  (lambda (item lst)
    (cond
     ((null? lst) 0)
     ((equal? item (car lst)) (+ 0 (count-background item (cdr lst)) ))
     (else (+ 1 (count-background item (cdr lst)))))
      
    )
  )

(count-background 'blue '(red white blue yellow blue red))
(count-background 'red '(white blue green))
(count-background 'white '())

(define list-front
  (lambda (lst i)
    (cond
     ((null? lst) (error "Error: length of" lst " is less than" i))
     ((= 0 i) '())
     (else
      (cons (car lst) (list-front (cdr lst) (sub1 i))))
     )
    )
  )

(list-front '(a b c d e f g) 4)
(list-front '(a b c) 4)

(define wrapa-left
  (lambda (word n)
    (cond
     ((= n 0) "")
     (else
      (begin
	(wrapa-left word (sub1 n))
	(display "(")))
     )
    )
  )
(define wrapa-right
  (lambda (word n)
    (cond
     ((= n 0) "")
     (else
      (begin
	(wrapa-right word (sub1 n))
	(display ")")))
     )
    )
  )


(define wrapa
  (lambda (word n)
    (cond
     ((= 0 n) (display word))
     (else (begin
	     (wrapa-left word n)
	     (display word)
	     (wrapa-right word n)))
     )
    )
  )

(wrapa 'gift 1)
(wrapa 'sandwitch 2)
(wrapa 'prisoner 5)
(wrapa 'moon 0)

(define multiple?
  (lambda (m n)
    (cond
     ((zero? n) #f)
     ((zero? (remainder m n)) #t)
     (else #f)
     )
    )
  )

(multiple? 7 2)
(multiple? 9 3)
(multiple? 5 0)

(define sum-of-odds
  (lambda (n)
    (cond
     ((= 0 n) 0)
     ((odd? n) (+ n (sum-of-odds (sub1 n))))
     (else (+ 0 (sum-of-odds (sub1 n)))))
    )
  )
(sum-of-odds 8)

(define n-tuple-check
  (lambda (lst len)
    (cond
     ((null? lst) 0)
     (else
      (+ (* (car lst) (expt 10 (sub1 len))) (n-tuple-check (cdr lst) (sub1 len)))))
    )
  )
  

(define n-tuple->integer
  (lambda (lst)
    (cond
     ((null? lst) (error "Error: bad arguments" lst))
     (else
      (n-tuple-check lst (length lst))))
    )
  )

(n-tuple->integer '(3 1 4 6))
(n-tuple->integer '(0))
(n-tuple->integer '(34 56))

(/ 1 3)

(make-ratl 3 5)

















