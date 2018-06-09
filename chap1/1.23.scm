;; The smallest divisor test checks if a number is divisible by
;; 2, 3, 4, 5, 6, .... However if a number is divisible by 2
;; then, as far as primality is concerned, it is unneccessary
;; to check if it is divisible by larger even numbers. So for
;; primality we just have to check if the number is divisible
;; by 2, 3, 5, 7, 9, .... This improved smallest divisor check
;; uses the next-test-divisor to do that.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " is a prime")
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (define (next-test-divisor)
      (cond ((= test-divisor 2) 3)
	    (else (+ test-divisor 1))))
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (next-test-divisor)))))
  (find-divisor 2))

(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (even? a) (divides? 2 a))
