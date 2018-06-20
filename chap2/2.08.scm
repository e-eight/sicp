(define (sub-interval x y)
  (let ((s1 (- (upper-bound x) (upper-bound y)))
	(s2 (- (upper-bound x) (lower-bound y)))
	(s3 (- (lower-bound x) (upper-bound y)))
	(s4 (- (lower-bound x) (lower-bound y))))
    (make-interval (min s1 s2 s3 s4)
		   (max s1 s2 s3 s4))))


(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))
