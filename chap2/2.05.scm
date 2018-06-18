;; The procedures car and cdr essentially amounts to prime factorizing the
;; integer representing the pair. For car the only allowed prime factor is 2,
;; and for cdr the only allowed prime factor is 3. 

(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (car z)
  (filter-iter even?
	       (lambda(x) (/ x 2))
	       z
	       inc
	       0))

(define (cdr z)
  (filter-iter multiple-of-3?
	       (lambda(x) (/ x 3))
	       z
	       inc
	       0))

(define (filter-iter filter term a next b)
  (if (filter a)
      (filter-iter filter term (term a) next (next b))
      b))

(define (fast-exp base pow)
  (cond ((= pow 0) 1)
	((even? pow) (square (fast-exp base (/ pow 2))))
	(else (* base (fast-exp base (- pow 1))))))

(define (multiple-of-3? x) (= (remainder x 3) 0))
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (inc x) (+ x 1))
