;; First we will implement the fast exponential method and then use that to
;; implement Alyssa's suggestion.

(define (fast-exp base exp)
  (cond ((= exp 0) 1)
	((even? exp) (square (fast-exp base (/ exp 2))))
	(else (* base (fast-exp base (- exp 1))))))

;; fast-exp needs two helper procedures even? and square.

(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))

;; The fast-expmod procedure

(define (fast-expmod base exp m)
  (remainder (fast-exp base exp) m))

;; The results for (fast-expmod 2 6 3), (fast-expmod 2 7 3), and (fast-expmod 2
;; 8 3) are 1, 2, and 3 respectively. Those are indeed what the values should be.
;; Thus fast-expmod works correctly. However the name is ill-suited and that
;; makes it unsuitable for the fast prime tester. This is because this
;; procedure first computes base^exp and then takes the remainder of the result
;; with respect to m. For large values of exp, base^exp can become very large
;; thereby slowing the process. However in the original implementation of
;; expmod, such a situation will not arise because the base^exp is never
;; calculated.
