(define (mul-interval x y)
  (let ((p1 (* (upper-bound x) (upper-bound y)))
	(p2 (* (upper-bound x) (lower-bound y)))
	(p3 (* (lower-bound x) (upper-bound y)))
	(p4 (* (lower-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "Division by zero")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))
