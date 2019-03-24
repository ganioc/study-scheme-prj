;; chap13.scm, simulation
;; select characteristics of the system to be modeled
;; define objects and actions, mimic the real world

;; see the effects of parameter changes wihout tinkering with
;; the actual system

(load "util.scm")

;; randomness
;; uniformly distributed random variable, random variable
(random 100)
(define unif-rand-var-0-1
  (let ((big 1000000))
    (lambda ()
      (/ (+ 1 (random big)) big))))

;; time between successive arrival of customers is random
;; exponential distribution
(define exponential-random-variable
  (lambda (mean)
    (* mean (- (log (unif-rand-var-0-1))))
    ))

(define arrival-time-generator
  (lambda (av-arr-time)
    (+ 1 (round (exponential-random-variable (- av-arr-time 1))))
    ))

;; normal distribution
;; mean, m
;; standard deviation , s, flatness of the bell-shaped curve
;; 2/3 , m +/- standard devi

(define normal-random-variable
  (lambda (mean std-dev)
    (letrec ((compute (lambda (i)
			(if (zero? i)
			    0
			    (+ (- (unif-rand-var-0-1 ) 0.5)
			       (compute (sub1 i)))))))
      (+ mean (* std-dev (compute 12))))
    ))

(define gallons-generator
  (lambda ()
    (max 1 (round (normal-random-variable 12 4)))))



;; test --------------------------------------------------
(display (exponential-random-variable 10))
(display (arrival-time-generator 100))

