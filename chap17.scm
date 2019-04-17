;; chap17
(load "util.scm")

(define message
  (lambda (direction value)
    (writeln "  " direction "ing attempt with value: " value)
    value))

(define attempt
  (lambda (n)
    (let ((receiver (lambda (proc) (list n proc))))
      (receiver (lambda (x) x)))))


;; (define countdown
;;   (lambda (n)
;;     (writeln "This only appears once")
;;     (let ((pair (message "Exit"
;; 			 (attempt (message "Enter" n)))))
;;       (let ((v (1st pair)))
;; 	(returner

(define receiver
  (lambda (continuation)
    (continuation continuation)))

(define tester
  (lambda (continuation)
    (display "beginning")
    (call-with-current-continuation continuation)
    (display "middle")
    (call-with-current-continuation continuation)
    (display "end")))

(tester (call-with-current-continuation receiver))
