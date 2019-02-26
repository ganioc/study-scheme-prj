(+ 1 1)
;; (bkpt 'debugmode)
(+ 20 2)

(define writeln
  (lambda x
    (for-each display x)))

(define maximum
  (lambda (n1 n2)
    (max n1 n2)))

(define minimum
  (lambda (ls)
    (min ls)))

