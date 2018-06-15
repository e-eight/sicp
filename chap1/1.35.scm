;; The golden ratio is defined as the root of the equation x^2 = x + 1. Dividing
;; both sides by x we get x = 1 + 1/x. Thus the golden ratio is the fixed point
;; of the transformation x -> 1 + 1/x.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess answer)
    (< (abs (- guess answer)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; Using this procedure with f as (lambda(x) (+ 1 (/ 1 x))) and 1.0 for the
;; first guess returns the value 1.61803, which is matches the value of the
;; golden ratio upto six significant figures.
