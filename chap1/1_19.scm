;; Fast fibonacci
;;
;; The n-th fibonacci number can be obtained by applying
;; a -> a + b, b -> a, n times, starting with a = 1, b = 0.
;; The final values of a and b will be the (n + 1)-th and
;; n-th fibonacci numbers respectively.
;;
;; This is a special case of the transformation T_{pq}, a ->
;; bq + aq + ap, b -> bp + aq. Applying this transformation
;; twice we get, a -> bq' + aq' + ap', b-> bp' + aq', where
;; p' = p^2 + q^2, and q' = q^2 + 2pq. This is similar to
;; doing the T_{p'q'} transformation once. And in doing so
;; we can find the n-th fibonacci number in logarithmic
;; number of steps as we will be taking advantage of
;; successive squaring.

(define (fib n)
  (define (fib-iter a b p q count)
    (define (even? x)
      (= (remainder x 2) 0))
    (define (square x)
      (* x x))
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
	             b
		     (+ (square p) (square q))
		     (+ (square q) (* 2 p q))
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- count 1)))))
  (fib-iter 1 0 0 1 n))
		
	  
