;; Filtered Accumulate

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter a null-value)
    (if (> a b)
	null-value
	(iter (next a)
	      (if (filter a)
		  (combiner (term a) null-value)
		  null-value))))
  (iter a null-value))


;; We will use filtered-accumulate to sum the squares of prime numbers in the
;; interval a to b.

(define (sum-of-prime-squares a b)
  (filtered-sum prime? square a inc b))

;; Filtered-sum is defined in terms of filtered-accumulate. 

(define (filtered-sum filter term a next b)
  (filtered-accumulate + 0 filter term a next b))

;; The filter prime? is the smallest divisor test from section 1.2.

(define (prime? n)
  (if (= n 1)
      false
      (= n (smallest-divisor n))))


;; We will also use filtered-accumulate to compute the product of all positive
;; numbers less than n that are relatively prime to n. The filter
;; relative-prime? is a check to see if gcd(x, n) = 1 for x < n.


(define (product-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-product relative-prime? id 1 inc n))

;; Like the filtered-sum, filtered-product is also defined in terms of
;; filtered-accumulate.

(define (filtered-product filter term a next b)
  (filtered-accumulate * 1 filter term a next b))


;; Helper procedures

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))
(define (inc x) (+ x 1))
(define (gcd a b)
  (if (= (remainder a b) 0)
      b
      (gcd b (remainder a b))))
(define (smallest-divisor x)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) x) x)
	  ((= (remainder x test-divisor) 0) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))
