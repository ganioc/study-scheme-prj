;; algorithm
;; input is a pair
(define writeln
  (lambda args
    (for-each display args)
    (newline)))

(define factor
  (lambda (ls m)
    (cond
     ((= 0 m) (writeln ls))
     ((< 0 m) '())
     (else (factor (cons m ls)  ))
     )))
     
     
(define ls-factors
  (lambda (ls n m)
    (display ls)
    (ls-factors1 (cons (- n 1) ls) m )

    
    ;; (writeln ls "," m )
    ;; (newline)
    ;; ;; (factor '(n) (- m n))
    ;; (ls-factors (cons (- m 1) ls)  m)
    )
  )

(ls-factors '() 5 5)

(display 'aaa)
