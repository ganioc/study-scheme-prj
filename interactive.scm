;;
(load "util.scm")

(define interact-square-num
  (lambda ()
    (newline)
    (display "Please input a number:")
    (let ((num (read)))
      (cond
       ((eq? num 'stop) (begin
			  (display "stopped")
			  (newline)))
       ((not (number? num)) (begin
			      (display "not a number")
			      (newline)
			      (interact-square-num)))
       (else (begin
	       (display "Square :")
	       (writeln (expt num 2))
	       (newline)
	       0
	       ))))
    ))

	
	
