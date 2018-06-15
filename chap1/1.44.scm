(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (smooth f)
  (define (average x y z)
    (/ (+ x y z) 3))
  (lambda(x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda(x) (f (g x))))

(define dx 0.00001)
