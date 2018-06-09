;; The number of steps in the smallest-divisor primality test
;; method grows as n^{1/2}. This program will check if that
;; corresponds to the time it takes for the program to run
;; on the machine.

(define (search-for-primes start end)
  (define (found-prime n)
    (cond ((> n end)
	   (newline) (display " search finished "))
	  ((even? n) (found-prime (+ n 1)))
	  (else (timed-prime-test n)
		(found-prime (+ n 2)))))
  (found-prime start))

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
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (even? a) (divides? 2 a))

;; Surprisingly in my test I found that the machine took the
;; same amount of time (0 ms) to find the three smallest primes
;; greater than 1000, 10000, 100000, and 1000000, respectively.
;; I suspect that it actually took different durations, but the
;; resolution of scheme's runtime is not sensitive enough.
