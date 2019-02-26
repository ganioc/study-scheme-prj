(define writeln
  (lambda x
    (for-each display x)
    (newline)))

(define factor
  (lambda (ls m)
    (cond
     ;; ((< m 0 ) (writeln "out of factor"))
     ((= m 0) (writeln ls))
     (else
      (top-loop ls (min (car ls) m) m))
     )
    ))
     
(define top-loop
  (lambda (ls s m)
    (cond
     ((= s 0) '())
     ;; ((null? ls) (begin
     ;; 		   (factor (cons s ls) (- m s))
     ;; 		   (top-loop ls (- s 1) m)))
     (else (begin
	     (factor (cons s ls) (- m s))
	     (top-loop ls (- s 1) m)))
     )))
;; This is an exercise problem from book <data structure>
;; It seems LISP has the most compact code realization than C language
;; Spectacular !

(define decompse
  (lambda (x)
    (top-loop '() x x)))

(decompse 6)
;; (min 4 5)
