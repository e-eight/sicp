(define (make-rat n d)
  (define (sign x)
    (if (< x 0)
	(- 1)
	1))
  (let ((g (gcd (abs n) (abs d))))
    (cons (* (sign n) (sign d) (abs (/ n g)))
	  (abs (/ d g)))))

(define (gcd a b)
  (if (= (remainder a b) 0)
      b
      (gcd b (remainder a b))))

(define (numer x) (car x))
(define (denom x) (cdr x))

  
