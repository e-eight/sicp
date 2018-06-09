(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))

;; Using this program, we find that 199 and 1999 are primes
;; since they are there own smallest divisors (excluding 1),
;; whereas the smallest divisor of 19999 is 7.

