```scheme
(define (cube-root x)
  (define (good-enough? guess newguess)
    (< (/ (abs (- guess newguess)) guess) 0.0001))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cube-root-iter guess)
    (if (good-enough? guess (improve guess))
	guess
	(cube-root-iter (improve guess))))
  (cube-root-iter 1.0))

(define (cube x)
  (* x x x))
```
