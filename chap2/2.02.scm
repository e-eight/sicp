;; Midpoint of a line segment, x_mid = (x_start + x_end) / 2,
;; y_mid = (y_start + y_end) / 2.

(define (midpoint-segment line)
  (define (average a b) (/ (+ a b) 2))
  (let ((start (start-segment line))
	(end (end-segment line)))
    (make-point (average (x-point start) (x-point end))
		(average (y-point start) (y-point end)))))

(define (make-segment start end)
  (cons start end))

(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
