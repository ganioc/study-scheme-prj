;;
(load "util.scm")

(define estimate-moves
  (lambda (n)
    (cond
     ((= n 1) 1)
     (else
      (+ 1 (* 2 (estimate-moves (sub1 n))))))
    ))





;; test
(estimate-moves 20)
(expt 2 20)
