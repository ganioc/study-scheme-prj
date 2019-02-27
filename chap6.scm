;;;

(load "util.scm")

(define string-insert
  (lambda (insrt strng n)
    (string-append
     (substring strng 0 n)
     insrt
     (substring strng n (string-length strng)))
    ))

(define substring?
  (lambda (sstr strng)
    (let ((len1 (string-length sstr))
	  (len2 (string-length strng)))
      (cond
       ((= len1 len2) (if (string=? sstr strng)
			  #t
			  #f))
       ((> len1 len2) #f)
       (else (if (string=? sstr
			   (substring strng 0 len1))
		 #t
		 (substring? sstr (substring strng 1 len2))))))
    ))

(define substring-ref
  (lambda (strng n)
    (substring strng n (add1 n))))

(define string-reverse
  (lambda (strng)
    (let ((strlen (string-length strng)))
      (cond
       ((= 0 strlen) "")
       (else (string-append
	      (substring-ref strng (- strlen 1))
	      (string-reverse (substring strng
					 0
					 (- strlen 1)))))))
    ))

(define palindrome
  (lambda (strng)
    (string=? strng (string-reverse strng))
    ))

(define mystery
  (lambda (pos-int)
    (letrec ((helper
	      (lambda (n count)
		(cond
		 ((= n 1)
		  (newline)
		  (writeln "It took " count " steps to get to 1."))
		 ((even? n)
		  (writeln count
			   ". We divide " n " by 2.")
		  (newline)
		  (helper (/ n 2) (add1 count)))
		 (else
		  (writeln count
			   ". We multiply " n " by 3 and add 1.")
		  (newline)
		  (helper (+ (* n 3) 1) (add1 count)))))))
      (helper pos-int 0))
    ))

(define tolerance 0.000005)

(define close-enough?
  (lambda (u v)
    (< (abs (- u v)) tolerance)))

(define square-root
  (lambda (a)
    (letrec
	((next-estimate
	  (lambda (u)
	    (let ((v (/ (+ u (/ a u)) 2)))
	      (if (close-enough? u v)
		  v
		  (next-estimate v))))))
      (next-estimate 1))))

(define square-root-exact
  (lambda (n)
    (exact->inexact (square-root n))))

(define round-n-places
  (lambda (n dec-num)
    (let ((scale-factor (expt 10 n)))
      (/ (round (* dec-num scale-factor)) scale-factor))
    ))

(define read-demo
  (lambda ()
    (display "Enter data (enter done when finished):")
    (let ((response (read)))
      (cond
       ((eq? response 'done) (display "Thank you. Good bye."))
       (else (display "You entered: ")
	     (write response)
	     (newline)
	     (read-demo))))
    ))

(define greeting
  (lambda ()
    (writeln "Hello. How are you?")))

(define tower-of-hanoi
  (lambda (n)
    (letrec
	((move
	  (lambda (n source destination helper)
	    (if (= n 1)
		(list (list source destination))
		(append
		 (move (sub1 n) source helper destination)
		 (cons
		  (list source destination)
		  (move (sub1 n) helper destination source)))))))
      (move n 'L 'R 'C))
    ))

(define display-tower-of-hanoi
  (let ((show-move (lambda (s d)
		     (display s)
		     (display " --> ")
		     (display d))))
    (lambda (n)
      (letrec
	  ((move
	    (lambda ( n source destination helper)
	      (if (= n 1)
		  (begin
		    (show-move source destination)
		    (newline))
		  (begin
		    (move (sub1 n) source helper destination)
		    (show-move source destination)
		    (display ", ")
		    (move (sub1 n) helper destination source))))))
      (move n 'L 'R 'C)
      ))))

;; Whether add a new queen to a legal position list is legal
;; try is a new try queen
(define legal?
  (lambda (try legal-pl)
    (letrec
	((good?
	  (lambda (new-pl up down)
	    (cond
	     ((null? new-pl) #t)
	     (else (let
		       ((next-pos (car new-pl)))
		     (and
		      (not (= next-pos try))
		      (not (= next-pos up))
		      (not (= next-pos down))
		      (good? (cdr new-pl)
			     (add1 up)
			     (sub1 down)))))))))
      (good? legal-pl (add1 try) (sub1 try)))))

;; Check if it's a legal solution
(define solution?
  (lambda (legal-pl)
    (= (length legal-pl) 8)))

(define fresh-try 8)

;; 
(define build-solution
  (lambda (legal-pl)
    (writeln "Build-solution :" legal-pl)
    (newline)
    (cond
     ;; If it is a legal list
     ((solution? legal-pl) legal-pl)
     ;; 
     (else (forward fresh-try legal-pl)))
    ))
;; forward from 8 to 1
(define forward
  (lambda (try legal-pl)
    (cond
     ((zero? try) (backtrack legal-pl))
     ((legal? try legal-pl) (build-solution (cons try legal-pl)))
     (else (forward (sub1 try) legal-pl)))
    ))

(define backtrack
  (lambda (legal-pl)
    (writeln "Backtrack :" legal-pl)
    (newline)
    (cond
     ((null? legal-pl) '())
     (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))
    ))

(define build-all-solution
  (lambda ()
    (letrec
	((loop (lambda (sol)
		 (cond
		  ((null? sol) '())
		  (else (cons sol (loop (backtrack sol))))))))
      (loop (build-solution '())))))

(define searcher
  (lambda (legal? solution? fresh-try)
    (letrec
	((build-solution
	  (lambda (legal-pl)
	    (cond
	     ((solution? legal-pl) legal-pl)
	     (else (forward fresh-try legal-pl)))))
	 (forward
	  (lambda (try legal-pl)
	    (cond
	     ((zero? try) (backtrack legal-pl))
	     ((legal? try legal-pl)
	      (build-solution (cons try legal-pl)))
	     (else (forward (sub1 try) legal-pl)))))
	 (backtrack
	  (lambda (legal-pl)
	    (cond
	     ((null? legal-pl) '())
	     (else
	      (forward (sub1 (car legal-pl)) (cdr legal-pl))))))
	 (build-all-solutions
	  (lambda ()
	    (letrec
		((loop (lambda (sol)
			 (cond
			  ((null? sol) '())
			  (else (cons sol
				      (loop (backtrack sol))))))))
	      (loop (build-solution '()))))))
      (build-all-solutions))
    ))

	      
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(symbol->string 'hello)
(string-length "This is a string")
(string-append "This is" " a string ah")
(substring "This is a string" 0 4)
(string=? "this is a string" "this is a string")
(string-insert "4" "1235678" 3)

(substring? "s a s" "This is a string.")
(substring? "ringer" "This is a string.")
(substring? "" "This is a string..")

(string-reverse "This is a string")
(string-reverse "mom n dad")
(string-reverse "")
(string-reverse "Jack and Jill")

(palindrome "ok")
(palindrome "able was I ere I saw elba")
(mystery 4)
(mystery 13)
(square-root 2)
;; (exact->inexact (square-root 2))
(square-root-exact 100)
(square-root-exact 1000)
(begin
  (display "Is")
  (display " ")
  (display 1.4142))

(round-n-places 5 1.4142186)
(expt 10 2)
(/ (* 1.4142186 100) 100)
(round-n-places 2 (square-root-exact 2))

(write "Is")
;; (read-demo)
(display "Done")
;; Done
(greeting)
;; (read-demo)
;; 6.54
;; done

(tower-of-hanoi 3)
(display-tower-of-hanoi 3)
(forward 1 '())

(build-solution '())
;;(length (build-all-solution))

(searcher legal? (lambda (x) (= (length x) 7)) 7)
