;; Test of Carmichael numbers fooling Fermat test.

;; The expmod procedure from section 1.2.6 along with the helper functions
;; square and even?

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

;; The carmichael-test procedure will check if n passes Fermat test for every
;; a<n.

(define (carmichael-test n)
  (define (fermat-test a)
    (define (try-it a) (= (expmod a n n) a))
    (cond ((= a n) true)
	  ((try-it a) (fermat-test (+ a 1)))
	  (else false)))
  (fermat-test 0))

;; As claimed in the book, the Carmichael numbers indeed fool this test.
