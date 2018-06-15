;; n-th root

;; We want to find the n-th root of a number x by as the fixed point of the map
;; y -> x / y^{n-1}. We already know that for finding the square root of x by
;; this method we need to use average damping, once. We will now investigate how
;; many times do we have to average damp for the n-th root.

;; By experimentation we see that for square and cube roots we need one average
;; damping, for fourth to seventh roots we need to average damp twice, and for
;; eighth roots to fifteenth roots we need to average damp thrice.
;;
;; The pattern that emerges is for 'a' average damps we can compute all the
;; roots from the 2^{a}-th root to the (2^{a+1} - 1)-th root. Thus 2^a <= n <=
;; 2^{a+1} - 1. Taking log to the base two of this 'equation' we get, a <=
;; log2(n) <= log2(2^{a+1} - 1). Since a is an integer therefore a must be equal
;; to floor(log2(n)).

(define (n-root x n)
  (fixed-point
   ((repeated average-damp (floor (log2 n)))
    (lambda(y) (/ x (fast-exp y (- n 1)))))
    1.0))

(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda(x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? guess answer)
    (< (abs (- guess answer)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))


;; Helper procedures

(define tolerance 0.00001)

(define (fast-exp base pow)
  (cond ((= pow 0) 1)
	((even? pow) (square (fast-exp base (/ pow 2))))
	(else (* base (fast-exp base (- pow 1))))))
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))
(define (log2 x) (/ (log x) (log 2)))
