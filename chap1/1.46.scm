(define (iterative-improve good-enough? improve)
  (lambda(guess) (if (good-enough? guess)
		guess
		((iterative-improve good-enough? improve)
		 (improve guess)))))

(define (sqrt guess x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (define (average a b)
      (/ (+ a b) 2))
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) guess))
 
(define (fixed-point f guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) tolerance))
  ((iterative-improve good-enough? f) guess))

(define tolerance 0.00001)
