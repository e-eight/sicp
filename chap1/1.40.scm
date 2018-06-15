(define (cubic a b c)
  (lambda(x) (+ (* x x x) (* a x x) (* b x) c)))

(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda(x) (/ (g x) ((deriv g) x))))
  (fixed-point (newton-transform g) guess))

(define (deriv g)
    (lambda(x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (fixed-point f first-guess)
  (define (close-enough? guess answer)
    (< (abs (- guess answer)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define dx 0.00001)
(define tolerance 0.00001)
